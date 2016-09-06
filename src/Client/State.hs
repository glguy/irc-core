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

  -- * Lenses
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
  , clientActivityBar
  , clientSubfocus
  , clientNextConnectionId
  , clientNetworkMap
  , clientIgnores
  , clientConnection
  , clientBell
  , clientExtensions

  -- * Client operations
  , withClientState
  , clientStartExtensions
  , clientShutdown
  , clientPark
  , clientMatcher
  , urlPattern

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
  , clientHighlights
  , clientWindowNames
  , clientPalette

  -- * Add messages to buffers
  , recordChannelMessage
  , recordNetworkMessage
  , recordIrcMessage

  -- * Focus manipulation
  , changeFocus
  , changeSubfocus
  , returnFocus
  , advanceFocus
  , retreatFocus
  , jumpToActivity
  , jumpFocus

  -- * Scrolling
  , pageUp
  , pageDown

  -- * Extensions
  , ExtensionState
  , esActive

  ) where

import           Client.CApi
import           Client.Configuration
import           Client.Configuration.ServerSettings
import           Client.Image.Message
import           Client.Image.Palette
import           Client.Message
import           Client.Network.Async
import           Client.State.Channel
import qualified Client.State.EditBox as Edit
import           Client.State.Focus
import           Client.State.Network
import           Client.State.Window
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.DeepSeq
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Default.Class
import           Data.Foldable
import           Data.Either
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.IntMap (IntMap)
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time
import           Foreign.Ptr
import           Foreign.StablePtr
import           Graphics.Vty
import           Irc.Codes
import           Irc.Identifier
import           Irc.Message
import           Irc.RawIrcMsg
import           Irc.UserInfo
import           LensUtils
import           Network.Connection (ConnectionContext, initConnectionContext)
import           Text.Regex.TDFA
import           Text.Regex.TDFA.String (compile)
import           Text.Regex.TDFA.Text () -- RegexLike Regex Text orphan

-- | All state information for the IRC client
data ClientState = ClientState
  { _clientWindows           :: !(Map Focus Window) -- ^ client message buffers
  , _clientPrevFocus         :: !Focus              -- ^ previously focused buffer
  , _clientFocus             :: !Focus              -- ^ currently focused buffer
  , _clientSubfocus          :: !Subfocus           -- ^ sec

  , _clientConnections       :: !(IntMap NetworkState) -- ^ state of active connections
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
  , _clientActivityBar       :: !Bool                     -- ^ visible activity bar
  , _clientBell              :: !Bool                     -- ^ sound a bell next draw

  , _clientIgnores           :: !(HashSet Identifier)     -- ^ ignored nicknames

  , _clientExtensions        :: !ExtensionState
  }

data ExtensionState = ExtensionState
  { _esActive    :: [ActiveExtension]
  , _esMVar      :: MVar ClientState
  , _esStablePtr :: StablePtr (MVar ClientState)
  }

makeLenses ''ClientState
makeLenses ''ExtensionState

clientPark :: ClientState -> (Ptr () -> IO a) -> IO (ClientState, a)
clientPark st k =
  do let mvar = view (clientExtensions . esMVar) st
     putMVar mvar st
     let token = views (clientExtensions . esStablePtr) castStablePtrToPtr st
     res <- k token
     st' <- takeMVar mvar
     return (st', res)

-- | 'Traversal' for finding the 'NetworkState' associated with a given network
-- if that connection is currently active.
clientConnection ::
  Applicative f =>
  Text {- ^ network -} ->
  LensLike' f ClientState NetworkState
clientConnection network f st =
  case view (clientNetworkMap . at network) st of
    Nothing -> pure st
    Just i  -> clientConnections (ix i f) st

-- | The full top-most line that would be executed
clientFirstLine :: ClientState -> String
clientFirstLine = fst . Edit.shift . view (clientTextBox . Edit.content)

-- | The line under the cursor in the edit box.
clientLine :: ClientState -> (Int, String) {- ^ line number, line content -}
clientLine = views (clientTextBox . Edit.line) (\(Edit.Line n t) -> (n, t))

-- | Construct an initial 'ClientState' using default values.
withClientState :: Configuration -> (ClientState -> IO a) -> IO a
withClientState cfg k =

  withVty            $ \vty ->
  withExtensionState $ \exts ->

  do (width,height) <- displayBounds (outputIface vty)
     cxt            <- initConnectionContext
     events         <- atomically newTQueue
     k ClientState
        { _clientWindows           = _Empty # ()
        , _clientNetworkMap        = _Empty # ()
        , _clientIgnores           = view configIgnores cfg
        , _clientConnections       = _Empty # ()
        , _clientTextBox           = Edit.defaultEditBox
        , _clientWidth             = width
        , _clientHeight            = height
        , _clientVty               = vty
        , _clientEvents            = events
        , _clientPrevFocus         = Unfocused
        , _clientFocus             = Unfocused
        , _clientSubfocus          = FocusMessages
        , _clientConnectionContext = cxt
        , _clientConfig            = cfg
        , _clientScroll            = 0
        , _clientDetailView        = False
        , _clientActivityBar       = view configActivityBar cfg
        , _clientNextConnectionId  = 0
        , _clientBell              = False
        , _clientExtensions        = exts
        }

-- | Initialize a 'Vty' value and run a continuation. Shutdown the 'Vty'
-- once the continuation finishes.
withVty :: (Vty -> IO a) -> IO a
withVty = bracket (mkVty def{bracketedPasteMode = Just True}) shutdown

withExtensionState :: (ExtensionState -> IO a) -> IO a
withExtensionState k =
  do mvar <- newEmptyMVar
     bracket (newStablePtr mvar) freeStablePtr $ \stab ->
       k ExtensionState
         { _esActive    = []
         , _esMVar      = mvar
         , _esStablePtr = stab
         }

-- | Forcefully terminate the connection currently associated
-- with a given network name.
abortNetwork ::
  Text {- ^ network -} ->
  ClientState -> IO ClientState
abortNetwork network st =
  case preview (clientConnection network) st of
    Nothing -> return st
    Just cs -> do abortConnection ForcedDisconnect (view csSocket cs)
                  return $ set (clientNetworkMap . at network) Nothing st

-- | Add a message to the window associated with a given channel
recordChannelMessage ::
  Text       {- ^ network -} ->
  Identifier {- ^ channel -} ->
  ClientMessage ->
  ClientState -> ClientState
recordChannelMessage network channel msg st =
  recordWindowLine focus wl st
  where
    focus      = ChannelFocus network channel'
    wl         = toWindowLine rendParams importance msg

    rendParams = MessageRendererParams
      { rendStatusMsg   = statusModes
      , rendUserSigils  = computeMsgLineSigils network channel' msg st
      , rendNicks       = HashSet.fromList (channelUserList network channel' st)
      , rendMyNicks     = highlights
      , rendPalette     = clientPalette st
      , rendNickPadding = view (clientConfig . configNickPadding) st
      }

    -- on failure returns mempty/""
    possibleStatusModes     = view (clientConnection network . csStatusMsg) st
    (statusModes, channel') = splitStatusMsgModes possibleStatusModes channel
    importance              = msgImportance msg st
    highlights              = clientHighlightsNetwork network st


-- | Extract the status mode sigils from a message target.
splitStatusMsgModes ::
  [Char]               {- ^ possible modes              -} ->
  Identifier           {- ^ target                      -} ->
  ([Char], Identifier) {- ^ actual modes, actual target -}
splitStatusMsgModes possible ident = (Text.unpack modes, mkId ident')
  where
    (modes, ident') = Text.span (`elem` possible) (idText ident)


-- | Compute the importance of a message to be used when computing
-- change notifications in the client.
msgImportance :: ClientMessage -> ClientState -> WindowLineImportance
msgImportance msg st =
  let network = view msgNetwork msg
      me      = preview (clientConnection network . csNick) st
      highlights = clientHighlightsNetwork network st
      isMe x  = Just x == me
      checkTxt txt
        | any (\x -> HashSet.member (mkId x) highlights)
              (nickSplit txt) = WLImportant
        | otherwise           = WLNormal
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
                             (addToWindow wl) st')
           st chans
      where
        cfg   = view clientConfig st
        wl    = toWindowLine' cfg WLBoring msg
        chans = user
              : case preview (clientConnection network . csChannels) st of
                  Nothing -> []
                  Just m  -> [chan | (chan, cs) <- HashMap.toList m
                                   , HashMap.member user (view chanUsers cs) ]

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
recordNetworkMessage msg st = recordWindowLine focus wl st
  where
    network    = view msgNetwork msg
    focus      | Text.null network = Unfocused
               | otherwise         = NetworkFocus (view msgNetwork msg)
    importance = msgImportance msg st
    wl         = toWindowLine' cfg importance msg

    cfg        = view clientConfig st

-- | Record window line at the given focus creating the window if necessary
recordWindowLine ::
  Focus ->
  WindowLine ->
  ClientState -> ClientState
recordWindowLine focus wl =
  over (clientWindows . at focus)
       (\w -> Just $! addToWindow wl (fromMaybe emptyWindow w))

toWindowLine :: MessageRendererParams -> WindowLineImportance -> ClientMessage -> WindowLine
toWindowLine params importance msg = WindowLine
  { _wlBody       = view msgBody msg
  , _wlText       = msgText (view msgBody msg)
  , _wlImage      = mkImage NormalRender
  , _wlFullImage  = mkImage DetailedRender
  , _wlImportance = importance
  , _wlTimestamp  = zonedTimeToUTC (view msgTime msg)
  }
  where
    mkImage mode =
      force (msgImage mode (view msgTime msg) params (view msgBody msg))

-- | 'toWindowLine' but with mostly defaulted parameters.
toWindowLine' :: Configuration -> WindowLineImportance -> ClientMessage -> WindowLine
toWindowLine' config =
  toWindowLine defaultRenderParams
    { rendPalette     = view configPalette     config
    , rendNickPadding = view configNickPadding config
    , rendMyNicks     = view configExtraHighlights config
    }


-- | Function applied to the client state every redraw.
clientTick :: ClientState -> ClientState
clientTick = set clientBell False . markSeen


-- | Mark the messages on the current window as seen.
markSeen :: ClientState -> ClientState
markSeen st =
  case view clientSubfocus st of
    FocusMessages ->
       overStrict (clientWindows . ix (view clientFocus st)) windowSeen st
    _             -> st

-- | Add the textbox input to the edit history and clear the textbox.
consumeInput :: ClientState -> ClientState
consumeInput = over clientTextBox Edit.success

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

-- | Construct a text matching predicate used to filter the message window.
clientMatcher :: ClientState -> Text -> Bool
clientMatcher st =
  case break (==' ') (clientFirstLine st) of
    ("/grep" ,_:reStr) -> go True  reStr
    ("/grepi",_:reStr) -> go False reStr
    ("/url"  ,_      ) -> match urlPattern
    _                  -> const True
  where
    go sensitive reStr =
      case compile defaultCompOpt{caseSensitive=sensitive} defaultExecOpt reStr of
        Left{}  -> const True
        Right r -> match r :: Text -> Bool

urlPattern :: Regex
urlPattern = makeRegex
  ("https?://([[:alnum:]-]+\\.)*([[:alnum:]-]+)(:[[:digit:]]+)?(/[^[:space:]]*)"::String)

-- | Remove a network connection and unlink it from the network map.
-- This operation assumes that the networkconnection exists and should
-- only be applied once per connection.
removeNetwork :: NetworkId -> ClientState -> (NetworkState, ClientState)
removeNetwork networkId st =
  case (clientConnections . at networkId <<.~ Nothing) st of
    (Nothing, _  ) -> error "removeNetwork: network not found"
    (Just cs, st1) ->
      -- Only remove the network mapping if it hasn't already been replaced
      -- with a new one. This can happen during reconnect in particular.
      let network = view csNetwork cs in
      forOf (clientNetworkMap . at network) st1 $ \mb ->
        case mb of
          Just i | i == networkId -> (cs,Nothing)
          _                       -> (cs,mb)

-- | Start a new connection. The delay is used for reconnections.
addConnection ::
  Int           {- ^ attempts                 -} ->
  Maybe UTCTime {- ^ optional disconnect time -} ->
  Text          {- ^ network name             -} ->
  ClientState ->
  IO ClientState
addConnection attempts lastTime network st =
  do let defSettings = (view (clientConfig . configDefaults) st)
                     { _ssName = Just network
                     , _ssHostName = Text.unpack network
                     }

         settings = fromMaybe defSettings
                  $ preview (clientConfig . configServers . ix network) st

     let (i,st') = st & clientNextConnectionId <+~ 1
         -- don't bother delaying on the first reconnect
         delay = 15 * max 0 (attempts - 1)
     c <- createConnection
            delay
            i
            (view clientConnectionContext st')
            settings
            (view clientEvents st')

     let cs = newNetworkState i network settings c (PingConnecting attempts lastTime)
     traverse_ (sendMsg cs) (initialMessages cs)

     return $ set (clientNetworkMap . at network) (Just i)
            $ set (clientConnections . at i) (Just cs) st'

applyMessageToClientState ::
  ZonedTime                  {- ^ timestamp                -} ->
  IrcMsg                     {- ^ message recieved         -} ->
  NetworkId                  {- ^ message network          -} ->
  NetworkState               {- ^ network connection state -} ->
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

    moveWindow :: Map Focus Window -> Map Focus Window
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
  do let (aes,st1) = (clientExtensions . esActive <<.~ []) st
     (st2,_) <- clientPark st1 $ \ptr ->
                  traverse_ (deactivateExtension ptr) aes
     return st2

-- | Start extensions after ensuring existing ones are stopped
clientStartExtensions :: ClientState -> IO ClientState
clientStartExtensions st =
  do let cfg = view clientConfig st
     st1        <- clientStopExtensions st
     (st2, res) <- clientPark st1 $ \ptr ->
            traverse (try . activateExtension ptr <=< resolveConfigurationPath)
                     (view configExtensions cfg)

     let (errors, exts) = partitionEithers res
     st3 <- recordErrors errors st2
     return $! set (clientExtensions . esActive) exts st3
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

------------------------------------------------------------------------
-- Scrolling
------------------------------------------------------------------------

-- | Scroll the current buffer to show older messages
pageUp :: ClientState -> ClientState
pageUp st = over clientScroll (+ scrollAmount st) st

-- | Scroll the current buffer to show newer messages
pageDown :: ClientState -> ClientState
pageDown st = over clientScroll (max 0 . subtract (scrollAmount st)) st

-- | Compute the number of lines in a page at the current window size
scrollAmount :: ClientState -> Int
scrollAmount st = max 1 (view clientHeight st - 2)


------------------------------------------------------------------------
-- Focus Management
------------------------------------------------------------------------

-- | Jump the focus of the client to a buffer that has unread activity.
-- Some events like errors or chat messages mentioning keywords are
-- considered important and will be jumped to first.
jumpToActivity :: ClientState -> ClientState
jumpToActivity st =
  case mplus highPriority lowPriority of
    Just (focus,_) -> changeFocus focus st
    Nothing        -> st
  where
    windowList   = views clientWindows Map.toAscList st
    highPriority = find (view winMention . snd) windowList
    lowPriority  = find (\x -> view winUnread (snd x) > 0) windowList

-- | Jump the focus directly to a window based on its zero-based index.
jumpFocus ::
  Int {- ^ zero-based window index -} ->
  ClientState -> ClientState
jumpFocus i st
  | 0 <= i, i < Map.size windows = changeFocus focus st
  | otherwise                    = st
  where
    windows = view clientWindows st
    (focus,_) = Map.elemAt i windows

-- | Change the window focus to the given value, reset the subfocus
-- to message view, reset the scroll, remember the previous focus
-- if it changed.
changeFocus ::
  Focus       {- ^ new focus    -} ->
  ClientState {- ^ client state -} ->
  ClientState
changeFocus focus st
  = set clientScroll 0
  . updatePrevious
  . set clientFocus focus
  . set clientSubfocus FocusMessages
  $ st
  where
    oldFocus = view clientFocus st
    updatePrevious
      | focus == oldFocus = id
      | otherwise         = set clientPrevFocus oldFocus

-- | Change the subfocus to the given value, preserve the focus, reset
-- the scroll.
changeSubfocus ::
  Subfocus    {- ^ new subfocus -} ->
  ClientState {- ^ client state -} ->
  ClientState
changeSubfocus focus
  = set clientScroll 0
  . set clientSubfocus focus

-- | Return to previously focused window.
returnFocus :: ClientState -> ClientState
returnFocus st = changeFocus (view clientPrevFocus st) st

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

-- | Compute the set of extra identifiers that should be highlighted given
-- a particular network state.
clientHighlights ::
  NetworkState       {- ^ network state               -} ->
  ClientState        {- ^ client state                -} ->
  HashSet Identifier {- ^ extra highlight identifiers -}
clientHighlights cs st =
  HashSet.insert
    (view csNick cs)
    (view (clientConfig . configExtraHighlights) st)

-- | Compute the set of extra identifiers that should be highlighted given
-- a particular network.
clientHighlightsNetwork ::
  Text               {- ^ network                     -} ->
  ClientState        {- ^ client state                -} ->
  HashSet Identifier {- ^ extra highlight identifiers -}
clientHighlightsNetwork network st =
  case preview (clientConnection network) st of
    Just cs -> clientHighlights cs st
    Nothing -> view (clientConfig . configExtraHighlights) st

-- | Produce the list of window names configured for the client.
clientWindowNames ::
  ClientState ->
  [Char]
clientWindowNames = views (clientConfig . configWindowNames) Text.unpack

-- | Produce the list of window names configured for the client.
clientPalette :: ClientState -> Palette
clientPalette = view (clientConfig . configPalette)
