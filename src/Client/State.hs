{-# Language TemplateHaskell, BangPatterns, OverloadedStrings #-}
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
    ClientState(..)
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
  , clientBell
  , clientExtensions
  , initialClientState
  , clientShutdown
  , clientStartExtensions

  -- * Client operations
  , clientMatcher
  , consumeInput
  , currentCompletionList
  , ircIgnorable
  , clientFirstLine
  , clientLine
  , abortNetwork
  , addConnection
  , removeNetwork
  , clientTick
  , applyMessageToClientState

  -- * Add messages to buffers
  , recordChannelMessage
  , recordNetworkMessage
  , recordIrcMessage

  -- * Focus information
  , ClientFocus(..)
  , _ChannelFocus
  , _NetworkFocus
  , _Unfocused
  , ClientSubfocus(..)
  , focusNetwork
  , changeFocus
  , changeSubfocus
  , advanceFocus
  , retreatFocus

  ) where

import {-# SOURCE #-} Client.CApi
import           Client.ChannelState
import           Client.Configuration
import           Client.ConnectionState
import qualified Client.EditBox as Edit
import           Client.Image.Message
import           Client.Message
import           Client.NetworkConnection
import           Client.ServerSettings
import           Client.Window
import           Control.Concurrent.STM
import           Control.DeepSeq
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.Either
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time
import           Graphics.Vty
import           Irc.Codes
import           Irc.Identifier
import           Irc.Message
import           Irc.RawIrcMsg
import           Irc.UserInfo
import           LensUtils
import           Text.Regex.TDFA
import           Text.Regex.TDFA.String (compile)
import           Text.Regex.TDFA.Text () -- RegexLike Regex Text orphan
import           Network.Connection

-- | Currently focused window
data ClientFocus
 = Unfocused                      -- ^ No network
 | NetworkFocus !Text             -- ^ Network
 | ChannelFocus !Text !Identifier -- ^ Network Channel/Nick
  deriving Eq

makePrisms ''ClientFocus

-- | Subfocus for a channel view
data ClientSubfocus
  = FocusMessages    -- ^ Show chat messages
  | FocusInfo        -- ^ Show channel metadata
  | FocusUsers       -- ^ Show user list
  | FocusMasks !Char -- ^ Show mask list for given mode
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
  , _clientEvents            :: !(TQueue NetworkEvent)    -- ^ incoming network event queue
  , _clientNetworkMap        :: !(HashMap Text NetworkId) -- ^ network name to connection ID

  , _clientVty               :: !Vty                      -- ^ VTY handle
  , _clientTextBox           :: !Edit.EditBox             -- ^ primary text box
  , _clientWidth             :: !Int                      -- ^ current terminal width
  , _clientHeight            :: !Int                      -- ^ current terminal height
  , _clientConfig            :: !Configuration            -- ^ client configuration
  , _clientScroll            :: !Int                      -- ^ buffer scroll lines
  , _clientDetailView        :: !Bool                     -- ^ use detailed rendering mode
  , _clientBell              :: !Bool                     -- ^ sound a bell next draw

  , _clientIgnores           :: !(HashSet Identifier)     -- ^ ignored nicknames

  , _clientExtensions        :: [ActiveExtension]       -- ^ Active extensions
  }

makeLenses ''ClientState

-- | 'Traversal' for finding the 'ConnectionState' associated with a given network
-- if that connection is currently active.
clientConnection ::
  Applicative f =>
  Text {- ^ network -} ->
  LensLike' f ClientState ConnectionState
clientConnection network f st =
  case view (clientNetworkMap . at network) st of
    Nothing -> pure st
    Just i  -> clientConnections (ix i f) st

-- | Full content of the edit box
clientFirstLine :: ClientState -> String
clientFirstLine = views (clientTextBox . Edit.content) Edit.firstLine

-- | The line under the cursor in the edit box.
clientLine :: ClientState -> (Int, String) {- ^ line number, line content -}
clientLine = views (clientTextBox . Edit.line) (\(Edit.Line n t) -> (n, t))

-- | Return the network associated with the current focus
focusNetwork :: ClientFocus -> Maybe Text {- ^ network -}
focusNetwork Unfocused = Nothing
focusNetwork (NetworkFocus network) = Just network
focusNetwork (ChannelFocus network _) = Just network

-- | Construct an initial 'ClientState' using default values.
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
        , _clientBell              = False
        , _clientExtensions        = []
        }

-- | Forcefully terminate the connection currently associated
-- with a given network name.
abortNetwork ::
  Text {- ^ network -} ->
  ClientState -> IO ClientState
abortNetwork network st =
  case preview (clientConnection network) st of
    Nothing -> return st
    Just cs -> do abortConnection (view csSocket cs)
                  return $ set (clientNetworkMap . at network) Nothing st

-- | Add a message to the window associated with a given channel
recordChannelMessage ::
  Text       {- ^ network -} ->
  Identifier {- ^ channel -} ->
  ClientMessage ->
  ClientState -> ClientState
recordChannelMessage network channel msg st =
  recordWindowLine focus importance wl st
  where
    focus      = ChannelFocus network channel'
    wl         = toWindowLine rendParams msg

    rendParams = MessageRendererParams
      { rendStatusMsg   = statusModes
      , rendUserSigils  = computeMsgLineSigils network channel' msg st
      , rendNicks       = channelUserList network channel' st
      , rendMyNicks     = toListOf (clientConnection network . csNick) st
      , rendPalette     = view (clientConfig . configPalette) st
      , rendNickPadding = view (clientConfig . configNickPadding) st
      }

    -- on failure returns mempty/""
    possibleStatusModes     = view (clientConnection network . csStatusMsg) st
    (statusModes, channel') = splitStatusMsgModes possibleStatusModes channel
    importance              = msgImportance msg st

-- | Compute the importance of a message to be used when computing
-- change notifications in the client.
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
    NormalBody{} -> WLImportant
    ErrorBody{}  -> WLImportant
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
        Ctcp _ tgt "ACTION" txt
          | isMe tgt  -> WLImportant
          | otherwise -> checkTxt txt
        Ctcp{} -> WLNormal
        Part who _ _ | isMe (userNick who) -> WLImportant
                     | otherwise           -> WLBoring
        Kick _ _ kicked _ | isMe kicked -> WLImportant
                          | otherwise   -> WLNormal
        Error{} -> WLImportant
        Reply cmd _ ->
          case replyCodeType (replyCodeInfo cmd) of
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
    -- privmsg ctcp commands are already metadata
    Ctcp who _ "ACTION" _ -> checkUser who
    -- notice ctcp responses are not already metadata
    CtcpNotice who _ _ _ -> checkUser who
    _               -> Nothing
  where
    ignores = view clientIgnores st
    checkUser !who
      | HashSet.member (userNick who) ignores = Just (userNick who)
      | otherwise                             = Nothing


-- | Record a message in the windows corresponding to the given target
recordIrcMessage ::
  Text {- ^ network -} ->
  MessageTarget ->
  ClientMessage ->
  ClientState -> ClientState
recordIrcMessage network target msg st =
  case target of
    TargetHidden      -> st
    TargetNetwork     -> recordNetworkMessage msg st
    TargetWindow chan -> recordChannelMessage network chan msg st
    TargetUser user   ->
      foldl' (\st' chan -> overStrict
                             (clientWindows . ix (ChannelFocus network chan))
                             (addToWindow WLBoring wl) st')
           st chans
      where
        cfg   = view clientConfig st
        wl    = toWindowLine' cfg msg
        chans = user
              : case preview (clientConnection network . csChannels) st of
                  Nothing -> []
                  Just m  -> [chan | (chan, cs) <- HashMap.toList m
                                   , HashMap.member user (view chanUsers cs) ]

-- | Extract the status mode sigils from a message target.
splitStatusMsgModes ::
  [Char]               {- ^ possible modes              -} ->
  Identifier           {- ^ target                      -} ->
  ([Char], Identifier) {- ^ actual modes, actual target -}
splitStatusMsgModes possible ident = (Text.unpack modes, mkId ident')
  where
    (modes, ident') = Text.span (`elem` possible) (idText ident)

-- | Compute the sigils of the user who sent a message.
computeMsgLineSigils ::
  Text       {- ^ network -} ->
  Identifier {- ^ channel -} ->
  ClientMessage ->
  ClientState ->
  [Char] {- ^ sigils -}
computeMsgLineSigils network channel msg st =
  case msgActor =<< preview (msgBody . _IrcBody) msg of
    Just user -> computeUserSigils network channel (userNick user) st
    Nothing   -> []

-- | Compute sigils for a user on a channel
computeUserSigils ::
  Text       {- ^ network -} ->
  Identifier {- ^ channel -} ->
  Identifier {- ^ user    -} ->
  ClientState ->
  [Char] {- ^ sigils -}
computeUserSigils network channel user =
    view $ clientConnection network
         . csChannels . ix channel
         . chanUsers  . ix user

-- | Record a message on a network window
recordNetworkMessage :: ClientMessage -> ClientState -> ClientState
recordNetworkMessage msg st = recordWindowLine focus importance wl st
  where
    network    = view msgNetwork msg
    focus      | Text.null network = Unfocused
               | otherwise         = NetworkFocus (view msgNetwork msg)
    importance = msgImportance msg st
    wl         = toWindowLine' cfg msg

    cfg        = view clientConfig st

-- | Record window line at the given focus creating the window if necessary
recordWindowLine ::
  ClientFocus ->
  WindowLineImportance ->
  WindowLine ->
  ClientState -> ClientState
recordWindowLine focus importance wl =
  over (clientWindows . at focus)
       (\w -> Just $! addToWindow importance wl (fromMaybe emptyWindow w))

toWindowLine :: MessageRendererParams -> ClientMessage -> WindowLine
toWindowLine params msg = WindowLine
  { _wlBody      = view msgBody msg
  , _wlText      = msgText (view msgBody msg)
  , _wlImage     = mkImage NormalRender
  , _wlFullImage = mkImage DetailedRender
  }
  where
    mkImage mode =
      force (msgImage mode (view msgTime msg) params (view msgBody msg))

-- | 'toWindowLine' but with mostly defaulted parameters.
toWindowLine' :: Configuration -> ClientMessage -> WindowLine
toWindowLine' config =
  toWindowLine defaultRenderParams
    { rendPalette     = view configPalette     config
    , rendNickPadding = view configNickPadding config
    }


-- | Function applied to the client state every redraw.
clientTick :: ClientState -> ClientState
clientTick = set clientBell False . markSeen


-- | Mark the messages on the current window as seen.
markSeen :: ClientState -> ClientState
markSeen st =
  case view clientSubfocus st of
    FocusMessages -> overStrict (clientWindows . ix (view clientFocus st)) windowSeen st
    _             -> st

-- | Add the textbox input to the edit history and clear the textbox.
consumeInput :: ClientState -> ClientState
consumeInput = over clientTextBox Edit.success

-- | Step focus to the next window when on message view. Otherwise
-- switch to message view.
advanceFocus :: ClientState -> ClientState
advanceFocus = stepFocus False

-- | Step focus to the previous window when on message view. Otherwise
-- switch to message view.
retreatFocus :: ClientState -> ClientState
retreatFocus = stepFocus True

-- | Step focus to the next window when on message view. Otherwise
-- switch to message view. Reverse the step order when argument is 'True'.
stepFocus :: Bool {- ^ reversed -} -> ClientState -> ClientState
stepFocus isReversed st
  | view clientSubfocus st /= FocusMessages = changeSubfocus FocusMessages st

  | isReversed, Just ((k,_),_) <- Map.maxViewWithKey l = changeFocus k st
  | isReversed, Just ((k,_),_) <- Map.maxViewWithKey r = changeFocus k st

  | isForward , Just ((k,_),_) <- Map.minViewWithKey r = changeFocus k st
  | isForward , Just ((k,_),_) <- Map.minViewWithKey l = changeFocus k st

  | otherwise                                          = st
  where
    isForward = not isReversed
    (l,r)     = Map.split (view clientFocus st) (view clientWindows st)

-- | Returns the current network's channels and current channel's users.
currentCompletionList :: ClientState -> [Identifier]
currentCompletionList st =
  case view clientFocus st of
    NetworkFocus network      -> networkChannelList network st
    ChannelFocus network chan -> networkChannelList network st
                              ++ channelUserList network chan st
    _                         -> []

networkChannelList ::
  Text         {- ^ network -} ->
  ClientState                  ->
  [Identifier] {- ^ channels -}
networkChannelList network =
  views (clientConnection network . csChannels) HashMap.keys

channelUserList ::
  Text         {- ^ network -} ->
  Identifier   {- ^ channel -} ->
  ClientState                  ->
  [Identifier] {- ^ nicks   -}
channelUserList network channel =
  views (clientConnection network . csChannels . ix channel . chanUsers) HashMap.keys

changeFocus :: ClientFocus -> ClientState -> ClientState
changeFocus focus
  = set clientScroll 0
  . set clientFocus focus
  . set clientSubfocus FocusMessages

changeSubfocus :: ClientSubfocus -> ClientState -> ClientState
changeSubfocus focus
  = set clientScroll 0
  . set clientSubfocus focus

-- | Construct a text matching predicate used to filter the message window.
clientMatcher :: ClientState -> Text -> Bool
clientMatcher st =
  case break (==' ') (clientFirstLine st) of
    ("/grep" ,_:reStr) -> go True  reStr
    ("/grepi",_:reStr) -> go False reStr
    _                  -> const True
  where
    go sensitive reStr =
      case compile defaultCompOpt{caseSensitive=sensitive} defaultExecOpt reStr of
        Left{}  -> const True
        Right r -> match r :: Text -> Bool

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
  do let defSettings = (view (clientConfig . configDefaults) st)
                     { _ssName = Just network
                     , _ssHostName = Text.unpack network
                     }

         settings = fromMaybe defSettings
                  $ preview (clientConfig . configServers . ix network) st

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

applyMessageToClientState ::
  ZonedTime                  {- ^ timestamp                -} ->
  IrcMsg                     {- ^ message recieved         -} ->
  NetworkId                  {- ^ message network          -} ->
  ConnectionState            {- ^ network connection state -} ->
  ClientState                                                 ->
  ([RawIrcMsg], ClientState) {- ^ response , updated state -}
applyMessageToClientState time irc networkId cs st =
  cs' `seq` (reply, st')
  where
    (reply, cs') = applyMessage time irc cs
    network      = view csNetwork cs
    st'          = applyWindowRenames network irc
                 $ set (clientConnections . ix networkId) cs' st

-- | When a nick change happens and there is an open query window for that nick
-- and there isn't an open query window for the new nick, rename the window.
applyWindowRenames ::
  Text {- ^ network -} ->
  IrcMsg               ->
  ClientState -> ClientState
applyWindowRenames network (Nick old new) st
  | hasWindow old'
  , not (hasWindow new) = over clientFocus moveFocus
                        $ over clientWindows moveWindow st
  | otherwise = st
  where
    old' = userNick old

    mkFocus = ChannelFocus network

    hasWindow who = has (clientWindows . ix (mkFocus who)) st

    moveWindow :: Map ClientFocus Window -> Map ClientFocus Window
    moveWindow wins =
      let (win,wins') = (at (mkFocus old') <<.~ Nothing) wins
      in set (at (mkFocus new)) win wins'

    moveFocus x
      | x == mkFocus old' = mkFocus new
      | otherwise         = x

applyWindowRenames _ _ st = st

clientShutdown :: ClientState -> IO ()
clientShutdown st = () <$ clientStopExtensions st
 -- other shutdown stuff might be added here later

clientStopExtensions :: ClientState -> IO ClientState
clientStopExtensions st =
  do let (aes,st1) = (clientExtensions <<.~ []) st
     (st2,_) <- withStableMVar st1 $ \ptr ->
                  traverse_ (deactivateExtension ptr) aes
     return st2

-- | Start extensions after ensuring existing ones are stopped
clientStartExtensions :: ClientState -> IO ClientState
clientStartExtensions st =
  do let cfg = view clientConfig st
     st1        <- clientStopExtensions st
     (st2, res) <- withStableMVar st1 $ \ptr ->
            traverse (try . activateExtension ptr <=< resolveConfigurationPath)
                     (view configExtensions cfg)

     let (errors, exts) = partitionEithers res
     st3 <- recordErrors errors st2
     return $! set clientExtensions exts st3
  where
    recordErrors [] ste = return ste
    recordErrors es ste =
      do now <- getZonedTime
         return $! foldl' (recordError now) ste es

    recordError now ste e =
      recordNetworkMessage ClientMessage
        { _msgTime    = now
        , _msgBody    = ErrorBody (Text.pack (show (e :: IOError)))
        , _msgNetwork = ""
        } ste
