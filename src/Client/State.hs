{-# Language TemplateHaskell, BangPatterns #-}
{-|
Module      : Client.State
Description : Primary client state type and update operations
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the core logic of the IRC client. The client
state tracks everything about the client.

-}
module Client.State
  (
  -- * Client state type
    NetworkName
  , ClientState(..)
  , clientWindows
  , clientTextBox
  , clientConnections
  , clientWidth
  , clientHeight
  , clientEvents
  , clientVty
  , clientFocus
  , clientConnectionContext
  , clientConfig
  , clientScroll
  , clientDetailView
  , clientSubfocus
  , clientNextConnectionId
  , clientNetworkMap
  , clientIgnores
  , clientConnection
  , initialClientState

  -- * Client operations
  , clientMatcher
  , consumeInput
  , currentUserList
  , ircIgnorable
  , clientInput
  , abortNetwork
  , addConnection
  , removeNetwork
  , clientTick

  -- * Add messages to buffers
  , recordChannelMessage
  , recordNetworkMessage
  , recordIrcMessage

  -- * Focus information
  , ClientFocus(..)
  , ClientSubfocus(..)
  , focusNetwork
  , changeFocus
  , changeSubfocus
  , advanceFocus
  , retreatFocus
  , windowNames

  ) where

import           Client.ChannelState
import           Client.Configuration
import           Client.ConnectionState
import           Client.Image.Message
import           Client.Message
import           Client.NetworkConnection
import           Client.ServerSettings
import           Client.Window
import           Control.Concurrent.STM
import           Control.Lens
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import           Data.HashSet (HashSet)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Maybe
import           Data.Map (Map)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.ICU as ICU
import           Data.Time
import           Graphics.Vty
import           Irc.Identifier
import           Irc.Message
import           Irc.UserInfo
import           Irc.Codes
import           LensUtils
import           Network.Connection
import qualified Client.EditBox as Edit
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map

type NetworkName = Text

data ClientFocus
 = Unfocused
 | NetworkFocus !NetworkName
 | ChannelFocus !NetworkName !Identifier
  deriving Eq

data ClientSubfocus
  = FocusMessages
  | FocusUsers
  | FocusMasks !Char
  deriving Eq

-- | Unfocused first, followed by focuses sorted by network.
-- Within the same network the network focus comes first and
-- then the channels are ordered by channel identifier
instance Ord ClientFocus where
  compare Unfocused            Unfocused            = EQ
  compare (NetworkFocus x)     (NetworkFocus y    ) = compare x y
  compare (ChannelFocus x1 x2) (ChannelFocus y1 y2) = compare x1 y1 <> compare x2 y2

  compare Unfocused _         = LT
  compare _         Unfocused = GT

  compare (NetworkFocus x  ) (ChannelFocus y _) = compare x y <> LT
  compare (ChannelFocus x _) (NetworkFocus y  ) = compare x y <> GT

-- | All state information for the IRC client
data ClientState = ClientState
  { _clientWindows           :: !(Map ClientFocus Window) -- ^ client message buffers
  , _clientFocus             :: !ClientFocus              -- ^ currently focused buffer
  , _clientSubfocus          :: !ClientSubfocus           -- ^ sec

  , _clientConnections       :: !(IntMap ConnectionState) -- ^ state of active connections
  , _clientNextConnectionId  :: !Int
  , _clientConnectionContext :: !ConnectionContext        -- ^ network connection context
  , _clientEvents            :: !(TQueue NetworkEvent)     -- ^ incoming network event queue
  , _clientNetworkMap        :: !(HashMap NetworkName NetworkId)
                                                          -- ^ network name to connection ID

  , _clientVty               :: !Vty                      -- ^ VTY handle
  , _clientTextBox           :: !Edit.EditBox             -- ^ primary text box
  , _clientWidth             :: !Int                      -- ^ current terminal width
  , _clientHeight            :: !Int                      -- ^ current terminal height
  , _clientConfig            :: !Configuration            -- ^ client configuration
  , _clientScroll            :: !Int                      -- ^ buffer scroll lines
  , _clientDetailView        :: !Bool                     -- ^ use detailed rendering mode

  , _clientIgnores           :: !(HashSet Identifier)     -- ^ ignored nicknames
  }

makeLenses ''ClientState

clientConnection :: Applicative f => NetworkName -> LensLike' f ClientState ConnectionState
clientConnection network f st =
  case view (clientNetworkMap . at network) st of
    Nothing -> pure st
    Just i  -> clientConnections (ix i f) st

clientInput :: ClientState -> String
clientInput = view (clientTextBox . Edit.content)

focusNetwork :: ClientFocus -> Maybe NetworkName
focusNetwork Unfocused = Nothing
focusNetwork (NetworkFocus network) = Just network
focusNetwork (ChannelFocus network _) = Just network

initialClientState :: Configuration -> Vty -> IO ClientState
initialClientState cfg vty =
  do (width,height) <- displayBounds (outputIface vty)
     cxt            <- initConnectionContext
     events         <- atomically newTQueue
     return ClientState
        { _clientWindows           = _Empty # ()
        , _clientTextBox           = Edit.empty
        , _clientConnections       = IntMap.empty
        , _clientWidth             = width
        , _clientHeight            = height
        , _clientVty               = vty
        , _clientEvents            = events
        , _clientFocus             = Unfocused
        , _clientConnectionContext = cxt
        , _clientConfig            = cfg
        , _clientScroll            = 0
        , _clientDetailView        = False
        , _clientSubfocus          = FocusMessages
        , _clientNextConnectionId  = 0
        , _clientNetworkMap        = HashMap.empty
        , _clientIgnores           = HashSet.empty
        }

abortNetwork :: NetworkName -> ClientState -> IO ClientState
abortNetwork network st =
  case preview (clientConnection network) st of
    Nothing -> return st
    Just cs -> do abortConnection (view csSocket cs)
                  return $ set (clientNetworkMap . at network) Nothing st

recordChannelMessage :: NetworkName -> Identifier -> ClientMessage -> ClientState -> ClientState
recordChannelMessage network channel msg st =
  over (clientWindows . at focus) (\w -> Just $! addToWindow importance wl (fromMaybe emptyWindow w)) st
  where
    focus = ChannelFocus network channel'
    wl = toWindowLine rendParams msg
    rendParams = MessageRendererParams
      { rendStatusMsg  = statusModes
      , rendUserSigils = computeMsgLineModes network channel' msg st
      , rendNicks      = channelUserList network channel' st
      }

    -- on failure returns mempty/""
    possibleStatusModes = view (clientConnection network . csStatusMsg) st
    (statusModes, channel') = splitStatusMsgModes possibleStatusModes channel
    importance = msgImportance msg st

msgImportance :: ClientMessage -> ClientState -> WindowLineImportance
msgImportance msg st =
  let network = view msgNetwork msg
      me      = preview (clientConnection network . csNick) st
      isMe x  = Just x == me
      checkTxt txt = case me of
                       Just me' | me' `elem` (mkId <$> nickSplit txt) -> WLImportant
                       _ -> WLNormal
  in
  case view msgBody msg of
    ExitBody    -> WLImportant
    ErrorBody _ -> WLImportant
    IrcBody irc
      | squelchIrcMsg irc -> WLBoring
      | isJust (ircIgnorable irc st) -> WLBoring
      | otherwise ->
      case irc of
        Privmsg _ tgt txt
          | isMe tgt  -> WLImportant
          | otherwise -> checkTxt txt
        Notice _ tgt txt
          | isMe tgt  -> WLImportant
          | otherwise -> checkTxt txt
        Action _ _  txt -> checkTxt txt
        Part who _ _ | isMe (userNick who) -> WLImportant
                     | otherwise           -> WLBoring
        Kick _ _ kicked _ | isMe kicked -> WLImportant
                          | otherwise   -> WLNormal
        Error{}         -> WLImportant
        Reply cmd _ ->
          case replyType cmd of
            ErrorReply -> WLImportant
            _          -> WLNormal
        _               -> WLBoring


-- | Predicate for messages that should be ignored based on the
-- configurable ignore list
ircIgnorable :: IrcMsg -> ClientState -> Maybe Identifier
ircIgnorable msg st =
  case msg of
    Privmsg who _ _ -> checkUser who
    Notice  who _ _ -> checkUser who
    _               -> Nothing
  where
    ignores = view clientIgnores st
    checkUser !who
      | HashSet.member (userNick who) ignores = Just (userNick who)
      | otherwise                             = Nothing


recordIrcMessage :: NetworkName -> MessageTarget -> ClientMessage -> ClientState -> ClientState
recordIrcMessage network target msg st =
  case target of
    TargetHidden -> st
    TargetNetwork -> recordNetworkMessage msg st
    TargetWindow chan -> recordChannelMessage network chan msg st
    TargetUser user -> foldl' (\st' chan -> overStrict
                                              (clientWindows . ix (ChannelFocus network chan))
                                              (addToWindow WLBoring wl) st')
                              st chans
      where
        wl = toWindowLine' msg
        chans =
          case preview (clientConnection network . csChannels) st of
            Nothing -> []
            Just m  -> [chan | (chan, cs) <- HashMap.toList m, HashMap.member user (view chanUsers cs) ]

splitStatusMsgModes :: [Char] -> Identifier -> ([Char], Identifier)
splitStatusMsgModes possible ident = (Text.unpack modes, mkId ident')
  where
    (modes, ident') = Text.span (`elem` possible) (idText ident)

computeMsgLineModes :: NetworkName -> Identifier -> ClientMessage -> ClientState -> [Char]
computeMsgLineModes network channel msg st =
  case msgActor =<< preview (msgBody . _IrcBody) msg of
    Just user -> computeLineModes network channel (userNick user) st
    Nothing   -> []

computeLineModes :: NetworkName -> Identifier -> Identifier -> ClientState -> [Char]
computeLineModes network channel user =
    view $ clientConnection network
         . csChannels . ix channel
         . chanUsers  . ix user

recordNetworkMessage :: ClientMessage -> ClientState -> ClientState
recordNetworkMessage msg st =
  over (clientWindows . at (NetworkFocus network))
       (\w -> Just $! addToWindow (msgImportance msg st) wl (fromMaybe emptyWindow w))
       st
  where
    network = view msgNetwork msg
    wl = toWindowLine' msg

toWindowLine :: MessageRendererParams -> ClientMessage -> WindowLine
toWindowLine params msg = WindowLine
  { _wlBody      = view msgBody msg
  , _wlText      = views msgBody msgText msg
  , _wlImage     = msgImage         (view msgTime msg) params (view msgBody msg)
  , _wlFullImage = detailedMsgImage (view msgTime msg) params (view msgBody msg)
  }

toWindowLine' :: ClientMessage -> WindowLine
toWindowLine' = toWindowLine defaultRenderParams

clientTick :: ClientState -> IO ClientState
clientTick st =
     return $! over (clientWindows . ix (view clientFocus st)) windowSeen st

consumeInput :: ClientState -> ClientState
consumeInput = over clientTextBox Edit.success

advanceFocus :: ClientState -> ClientState
advanceFocus st
  | view clientSubfocus st /= FocusMessages = changeSubfocus FocusMessages st
  | otherwise =
  case Map.split oldFocus windows of
    (l,r)
      | Just ((k,_),_) <- Map.minViewWithKey r -> success k
      | Just ((k,_),_) <- Map.minViewWithKey l -> success k
      | otherwise                              -> st
  where
    success x = set clientScroll 0
              $ set clientFocus x st
    oldFocus = view clientFocus st
    windows  = view clientWindows st

retreatFocus :: ClientState -> ClientState
retreatFocus st
  | view clientSubfocus st /= FocusMessages = changeSubfocus FocusMessages st
  | otherwise =
  case Map.split oldFocus windows of
    (l,r)
      | Just ((k,_),_) <- Map.maxViewWithKey l -> success k
      | Just ((k,_),_) <- Map.maxViewWithKey r -> success k
      | otherwise                              -> st
  where
    success x = set clientScroll 0
              $ set clientFocus x st
    oldFocus = view clientFocus st
    windows  = view clientWindows st

currentUserList :: ClientState -> [Identifier]
currentUserList st =
  case view clientFocus st of
    ChannelFocus network chan -> channelUserList network chan st
    _                         -> []

channelUserList :: NetworkName -> Identifier -> ClientState -> [Identifier]
channelUserList network channel st =
  views (clientConnection network . csChannels . ix channel . chanUsers) HashMap.keys st

changeFocus :: ClientFocus -> ClientState -> ClientState
changeFocus focus
  = set clientScroll 0
  . set clientFocus focus
  . set clientSubfocus FocusMessages

changeSubfocus :: ClientSubfocus -> ClientState -> ClientState
changeSubfocus focus
  = set clientScroll 0
  . set clientSubfocus focus

windowNames :: [Char]
windowNames = "1234567890qwertyuiop"

clientMatcher :: ClientState -> Text -> Bool
clientMatcher st =
  case break (==' ') (clientInput st) of
    ("/grep" ,_:reStr) -> go [] reStr
    ("/grepi",_:reStr) -> go [ICU.CaseInsensitive] reStr
    _                  -> const True
  where
    go opts reStr
      | not (null reStr)
      , Right r <- ICU.regex' opts (Text.pack reStr) = isJust . ICU.find r
      | otherwise                                    = const True

-- | Remove a network connection and unlink it from the network map.
-- This operation assumes that the networkconnection exists and should
-- only be applied once per connection.
removeNetwork :: NetworkId -> ClientState -> (ConnectionState, ClientState)
removeNetwork networkId st =
  case (clientConnections . at networkId <<.~ Nothing) st of
    (Nothing, _  ) -> error "removeNetwork: network not found"
    (Just cs, st1) ->
      -- Only remove the network mapping if it hasn't already been replace
      -- with a new one. This can happen during reconnect in particular.
      let network = view csNetwork cs in
      case view (clientNetworkMap . at network) st of
        Just i | i == networkId ->
          (cs, set (clientNetworkMap . at network) Nothing st1)
        _ -> (cs,st1)

addConnection :: Text -> ClientState -> IO ClientState
addConnection network st =
  do let host = Text.unpack network
         defSettings = (view (clientConfig . configDefaults) st)
                     { _ssHostName = host }
         settings = fromMaybe defSettings
                              (view (clientConfig . configServers . at host) st)

     let (i,st') = st & clientNextConnectionId <+~ 1
     c <- createConnection
            i
            (view clientConnectionContext st')
            settings
            (view clientEvents st')

     let cs = newConnectionState i network settings c
     traverse_ (sendMsg cs) (initialMessages cs)

     return $ set (clientNetworkMap . at network) (Just i)
            $ set (clientConnections . at i) (Just cs) st'

