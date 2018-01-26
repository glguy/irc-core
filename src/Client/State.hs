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
  , clientTextBoxOffset
  , clientConnections
  , clientWidth
  , clientHeight
  , clientEvents
  , clientFocus
  , clientPrevFocus
  , clientExtraFocus
  , clientConfig
  , clientScroll
  , clientDetailView
  , clientActivityBar
  , clientSubfocus
  , clientNetworkMap
  , clientIgnores
  , clientIgnoreMask
  , clientConnection
  , clientBell
  , clientExtensions
  , clientRegex
  , clientLogQueue
  , clientActivityReturn
  , clientErrorMsg
  , clientLayout
  , clientRtsStats
  , clientConfigPath

  -- * Client operations
  , withClientState
  , clientStartExtensions
  , clientShutdown
  , clientPark
  , clientMatcher
  , clientMatcher'
  , clientActiveRegex
  , clientToggleHideMeta
  , clientHighlightsNetwork
  , channelUserList

  , consumeInput
  , currentCompletionList
  , identIgnored
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
  , clientAutoconnects
  , clientActiveCommand

  , clientExtraFocuses
  , currentNickCompletionMode

  -- * Add messages to buffers
  , recordChannelMessage
  , recordNetworkMessage
  , recordIrcMessage

  -- * Focus manipulation
  , changeFocus
  , changeSubfocus
  , returnFocus
  , advanceFocus
  , advanceNetworkFocus
  , retreatFocus
  , jumpToActivity
  , jumpFocus
  , setExtraFocus

  -- * Scrolling
  , scrollClient

  -- * Extensions
  , ExtensionState
  , esActive

  -- * URL view
  , urlPattern
  , urlMatches

  ) where

import           Client.CApi
import           Client.Commands.WordCompletion
import           Client.Configuration
import           Client.Configuration.ServerSettings
import           Client.Image.Message
import           Client.Image.Palette
import           Client.Log
import           Client.Mask
import           Client.Message
import           Client.Network.Async
import           Client.State.Channel
import qualified Client.State.EditBox as Edit
import           Client.State.Focus
import           Client.State.Network
import           Client.State.Window
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
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
import           Data.List
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import           Data.Time
import           Foreign.Ptr
import           Foreign.StablePtr
import           Irc.Codes
import           Irc.Identifier
import           Irc.Message
import           Irc.RawIrcMsg
import           Irc.UserInfo
import           LensUtils
import           RtsStats (Stats)
import           Text.Regex.TDFA
import           Text.Regex.TDFA.String (compile)

-- | All state information for the IRC client
data ClientState = ClientState
  { _clientWindows           :: !(Map Focus Window) -- ^ client message buffers
  , _clientPrevFocus         :: !Focus              -- ^ previously focused buffer
  , _clientActivityReturn    :: !Focus              -- ^ focus prior to jumping to activity
  , _clientFocus             :: !Focus              -- ^ currently focused buffer
  , _clientSubfocus          :: !Subfocus           -- ^ current view mode
  , _clientExtraFocus        :: ![Focus]            -- ^ extra messages windows to view

  , _clientConnections       :: !(IntMap NetworkState) -- ^ state of active connections
  , _clientEvents            :: !(TQueue NetworkEvent)    -- ^ incoming network event queue
  , _clientNetworkMap        :: !(HashMap Text NetworkId) -- ^ network name to connection ID

  , _clientConfig            :: !Configuration            -- ^ client configuration
  , _clientConfigPath        :: !(Maybe FilePath)         -- ^ client configuration file path

  , _clientTextBox           :: !Edit.EditBox             -- ^ primary text box
  , _clientTextBoxOffset     :: !Int                      -- ^ size to crop from left of text box
  , _clientWidth             :: !Int                      -- ^ current terminal width
  , _clientHeight            :: !Int                      -- ^ current terminal height

  , _clientScroll            :: !Int                      -- ^ buffer scroll lines
  , _clientDetailView        :: !Bool                     -- ^ use detailed rendering mode
  , _clientActivityBar       :: !Bool                     -- ^ visible activity bar
  , _clientRegex             :: Maybe Regex               -- ^ optional persistent filter
  , _clientLayout            :: !LayoutMode               -- ^ layout mode for split screen

  , _clientBell              :: !Bool                     -- ^ sound a bell next draw

  , _clientIgnores           :: !(HashSet Identifier)     -- ^ ignored masks
  , _clientIgnoreMask        :: Mask                      -- ^ precomputed ignore regular expression (lazy)

  , _clientExtensions        :: !ExtensionState           -- ^ state of loaded extensions
  , _clientLogQueue          :: ![LogLine]                -- ^ log lines ready to write
  , _clientErrorMsg          :: Maybe Text                -- ^ transient error box text
  , _clientRtsStats          :: Maybe Stats               -- ^ most recent GHC RTS stats
  }


-- | State of the extension API including loaded extensions and the mechanism used
-- to support reentry into the Haskell runtime from the C API.
data ExtensionState = ExtensionState
  { _esActive    :: [ActiveExtension]            -- ^ active extensions
  , _esMVar      :: MVar ClientState             -- ^ 'MVar' used to with 'clientPark'
  , _esStablePtr :: StablePtr (MVar ClientState) -- ^ 'StablePtr' used with 'clientPark'
  }

makeLenses ''ClientState
makeLenses ''ExtensionState


-- | Prepare the client to support reentry from the extension API.
clientPark ::
  ClientState      {- ^ client state                                        -} ->
  (Ptr () -> IO a) {- ^ continuation using the stable pointer to the client -} ->
  IO (ClientState, a)
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
withClientState :: Maybe FilePath -> Configuration -> (ClientState -> IO a) -> IO a
withClientState cfgPath cfg k =

  withExtensionState $ \exts ->

  do events <- atomically newTQueue
     let ignoreIds = map mkId (view configIgnores cfg)
     k ClientState
        { _clientWindows           = _Empty # ()
        , _clientNetworkMap        = _Empty # ()
        , _clientIgnores           = HashSet.fromList ignoreIds
        , _clientIgnoreMask        = buildMask ignoreIds
        , _clientConnections       = _Empty # ()
        , _clientTextBox           = Edit.defaultEditBox
        , _clientTextBoxOffset     = 0
        , _clientWidth             = 80
        , _clientHeight            = 25
        , _clientEvents            = events
        , _clientPrevFocus         = Unfocused
        , _clientActivityReturn    = Unfocused
        , _clientFocus             = Unfocused
        , _clientSubfocus          = FocusMessages
        , _clientExtraFocus        = []
        , _clientConfig            = cfg
        , _clientConfigPath        = cfgPath
        , _clientScroll            = 0
        , _clientDetailView        = False
        , _clientRegex             = Nothing
        , _clientLayout            = view configLayout cfg
        , _clientActivityBar       = view configActivityBar cfg
        , _clientBell              = False
        , _clientExtensions        = exts
        , _clientLogQueue          = []
        , _clientErrorMsg          = Nothing
        , _clientRtsStats          = Nothing
        }

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
    Just cs -> do -- cancel the network thread
                  abortConnection ForcedDisconnect (view csSocket cs)
                  -- unassociate this network name from this network id
                  return $! over clientNetworkMap (sans network) st


-- | Add a message to the window associated with a given channel
recordChannelMessage ::
  Text       {- ^ network -} ->
  Identifier {- ^ channel -} ->
  ClientMessage ->
  ClientState ->
  ClientState
recordChannelMessage network channel msg st
  = recordLogLine msg channel
  $ recordWindowLine focus wl st
  where
    focus      = ChannelFocus network channel'
    wl         = toWindowLine rendParams importance msg

    rendParams = MessageRendererParams
      { rendStatusMsg   = statusModes
      , rendUserSigils  = computeMsgLineSigils network channel' msg st
      , rendNicks       = HashSet.fromList (channelUserList network channel' st)
      , rendMyNicks     = highlights
      , rendPalette     = clientPalette st
      }

    -- on failure returns mempty/""
    possibleStatusModes     = view (clientConnection network . csStatusMsg) st
    (statusModes, channel') = splitStatusMsgModes possibleStatusModes channel
    importance              = msgImportance msg st
    highlights              = clientHighlightsNetwork network st


recordLogLine ::
  ClientMessage {- ^ message      -} ->
  Identifier    {- ^ target       -} ->
  ClientState   {- ^ client state -} ->
  ClientState
recordLogLine msg target st =
  case view (clientConnection (view msgNetwork msg) . csSettings . ssLogDir) st of
    Nothing -> st
    Just dir ->
      case renderLogLine msg dir target of
        Nothing  -> st
        Just ll  -> over clientLogQueue (cons ll) st


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
ircIgnorable msg !st =
  case msg of
    Privmsg who _ _ -> checkUser who
    Notice  who _ _ -> checkUser who
    -- privmsg ctcp commands are already metadata
    Ctcp who _ "ACTION" _ -> checkUser who
    -- notice ctcp responses are not already metadata
    CtcpNotice who _ _ _ -> checkUser who
    _                    -> Nothing
  where
    checkUser !who
      | identIgnored who st = Just (userNick who)
      | otherwise           = Nothing



-- | Predicate for nicknames to determine if messages should be ignored.
identIgnored ::
  UserInfo    {- ^ target user  -} ->
  ClientState {- ^ client state -} ->
  Bool        {- ^ is ignored   -}
identIgnored who st = matchMask (view clientIgnoreMask st) who


-- | Record a message in the windows corresponding to the given target
recordIrcMessage ::
  Text {- ^ network -} ->
  MessageTarget ->
  ClientMessage ->
  ClientState -> ClientState
recordIrcMessage network target msg st =
  updateTransientError (NetworkFocus network) msg $
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


-- | Detect /error/ messages and add the message text to the transient
-- error display. The transient message will not be generated if the
-- user is focused on the window where the message is going to be
-- rendered, anyway.
updateTransientError :: Focus -> ClientMessage -> ClientState -> ClientState
updateTransientError destination msg st
  | view clientFocus st == destination = st
  | otherwise =

  let err e = set clientErrorMsg (Just e) st in

  case view msgBody msg of
    ErrorBody txt       -> err txt
    IrcBody (Error txt) -> err txt
    IrcBody (Reply code args)
      | let info = replyCodeInfo code
      , ErrorReply <- replyCodeType info ->
          err (Text.intercalate " " (replyCodeText info : drop 1 args))
    _ -> st


-- | Record a message on a network window
recordNetworkMessage :: ClientMessage -> ClientState -> ClientState
recordNetworkMessage msg st = updateTransientError focus msg
                            $ recordWindowLine focus wl st
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
  ClientState ->
  ClientState
recordWindowLine focus wl st = st2
  where
    freshWindow = emptyWindow { _winHideMeta = view (clientConfig . configHideMeta) st }
    st1 = over (clientWindows . at focus)
               (\w -> Just $! addToWindow wl (fromMaybe freshWindow w))
               st

    st2
      | not (view clientBell st)
      , view (clientConfig . configBellOnMention) st
      , view wlImportance wl == WLImportant
      , not (hasMention st) = set clientBell True st1

      | otherwise = st1

    hasMention = elemOf (clientWindows . folded . winMention) WLImportant

toWindowLine :: MessageRendererParams -> WindowLineImportance -> ClientMessage -> WindowLine
toWindowLine params importance msg = WindowLine
  { _wlSummary    = msgSummary (view msgBody msg)
  , _wlPrefix     = prefix
  , _wlImage      = image
  , _wlFullImage  = full
  , _wlImportance = importance
  , _wlTimestamp  = views msgTime packZonedTime msg
  }
  where
    (prefix, image, full) = msgImage (view msgTime msg) params (view msgBody msg)

-- | 'toWindowLine' but with mostly defaulted parameters.
toWindowLine' :: Configuration -> WindowLineImportance -> ClientMessage -> WindowLine
toWindowLine' config =
  toWindowLine defaultRenderParams
    { rendPalette     = view configPalette     config
    , rendMyNicks     = view configExtraHighlights config
    }


-- | Function applied to the client state every redraw.
clientTick :: ClientState -> ClientState
clientTick = set clientBell False
           . markSeen
           . set clientLogQueue []


-- | Mark the messages on the current window (and any splits) as seen.
markSeen :: ClientState -> ClientState
markSeen st =
  case view clientSubfocus st of
    FocusMessages -> foldl' aux st focuses
    _             -> st
  where
    aux acc focus = overStrict (clientWindows . ix focus) windowSeen acc

    focuses = view clientFocus st
            : view clientExtraFocus st

-- | Add the textbox input to the edit history and clear the textbox.
consumeInput :: ClientState -> ClientState
consumeInput = over clientTextBox Edit.success

-- | Returns the current network's channels and current channel's users.
currentCompletionList :: ClientState -> [Identifier]
currentCompletionList st =
  case view clientFocus st of
    NetworkFocus network      -> networkChannelList network st
    ChannelFocus network chan ->
         chan -- might be a disconnected channel or a private chat
       : networkChannelList network st
      ++ channelUserList network chan st
    _                         -> []

-- | Returns the 'WordCompletionMode' associated with the current network.
currentNickCompletionMode :: ClientState -> WordCompletionMode
currentNickCompletionMode st =
  fromMaybe defaultNickWordCompleteMode $
  do network <- views clientFocus focusNetwork st
     preview (clientConnection network . csSettings . ssNickCompletion) st

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


-- | Returns the current filtering predicate if one is active.
clientMatcher ::
  ClientState          {- ^ client state       -} ->
  Maybe (LText.Text -> Bool) {- ^ optional predicate -}
clientMatcher st =
  do r <- clientActiveRegex st
     return (matchTest r . LText.unpack)

-- | Strict version of 'clientMatcher'
clientMatcher' ::
  ClientState          {- ^ client state       -} ->
  Maybe (Text -> Bool) {- ^ optional predicate -}
clientMatcher' st =
  do p <- clientMatcher st
     return (p . LText.fromStrict)


-- | Construct a text matching predicate used to filter the message window.
clientActiveRegex :: ClientState -> Maybe Regex
clientActiveRegex st =
  case clientActiveCommand st of
    Just ("grep" ,reStr) -> go True  reStr
    Just ("grepi",reStr) -> go False reStr
    _ -> case view clientRegex st of
           Nothing -> Nothing
           Just r  -> Just r
  where
    go sensitive reStr =
      case compile defaultCompOpt{caseSensitive=sensitive}
                   defaultExecOpt{captureGroups=False}
                   reStr of
        Left{}  -> Nothing
        Right r -> Just r


-- | Compute the command and arguments currently in the textbox.
clientActiveCommand ::
  ClientState           {- ^ client state                     -} ->
  Maybe (String,String) {- ^ command name and argument string -}
clientActiveCommand st =
  case break (==' ') (clientFirstLine st) of
    ('/':cmd,_:args) -> Just (cmd,args)
    _                -> Nothing


-- | Regular expression for matching HTTP/HTTPS URLs in chat text.
urlPattern :: Regex
Right urlPattern =
  compile
    defaultCompOpt
    defaultExecOpt{captureGroups=False}
    "https?://([[:alnum:]-]+\\.)*([[:alnum:]-]+)(:[[:digit:]]+)?(/[^[:cntrl:][:space:]]*)|\
    \<https?://[^>]*>"


-- | Find all the URL matches using 'urlPattern' in a given 'Text' suitable
-- for being opened. Surrounding @<@ and @>@ are removed.
urlMatches :: LText.Text -> [Text]
urlMatches txt = removeBrackets . extractText . (^?! ix 0)
             <$> matchAll urlPattern (LText.unpack txt)
  where
    extractText (off,len) = LText.toStrict
                          $ LText.take (fromIntegral len)
                          $ LText.drop (fromIntegral off) txt

    removeBrackets t =
      case Text.uncons t of
       Just ('<',t') | not (Text.null t') -> Text.init t'
       _                                  -> t

-- | Remove a network connection and unlink it from the network map.
-- This operation assumes that the network connection exists and should
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

         i = nextAvailableKey (view clientConnections st)

         -- don't bother delaying on the first reconnect
         delay = 15 * max 0 (attempts - 1)

     c <- createConnection
            delay
            i
            settings
            (view clientEvents st)

     let cs = newNetworkState i network settings c (PingConnecting attempts lastTime)
     traverse_ (sendMsg cs) (initialMessages cs)

     return $ set (clientNetworkMap . at network) (Just i)
            $ set (clientConnections . at i) (Just cs) st

-- | Find the first unused key in the intmap starting at 0.
nextAvailableKey :: IntMap a -> Int
nextAvailableKey m = foldr aux id (IntMap.keys m) 0
  where
    aux k next i
      | k == i    = next (i+1)
      | otherwise = i

applyMessageToClientState ::
  ZonedTime                  {- ^ timestamp                -} ->
  IrcMsg                     {- ^ message received         -} ->
  NetworkId                  {- ^ message network          -} ->
  NetworkState               {- ^ network connection state -} ->
  ClientState                {- ^ client state             -} ->
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


-- | Actions to be run when exiting the client.
clientShutdown :: ClientState -> IO ()
clientShutdown st = () <$ clientStopExtensions st
 -- other shutdown stuff might be added here later


-- | Unload all active extensions.
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
            traverse (try . activateExtension ptr)
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

-- | Scroll the current buffer to show newer messages
scrollClient :: Int -> ClientState -> ClientState
scrollClient amt = over clientScroll $ \n -> max 0 (n + amt)


-- | List of extra focuses to display as split windows
clientExtraFocuses :: ClientState -> [Focus]
clientExtraFocuses st =
  case view clientSubfocus st of
    FocusMessages -> view clientFocus st `delete` view clientExtraFocus st
    _             -> []


------------------------------------------------------------------------
-- Focus Management
------------------------------------------------------------------------

-- | Jump the focus of the client to a buffer that has unread activity.
-- Some events like errors or chat messages mentioning keywords are
-- considered important and will be jumped to first.
jumpToActivity :: ClientState -> ClientState
jumpToActivity st = changeFocus newFocus st
  where
    windowList   = views clientWindows Map.toAscList st
    highPriority = find (\x -> WLImportant == view winMention (snd x)) windowList
    lowPriority  = find (\x -> view winUnread (snd x) > 0) windowList
    newFocus =
      case mplus highPriority lowPriority of
        Just (focus,_) -> focus
        Nothing        -> view clientActivityReturn st

-- | Jump the focus directly to a window based on its zero-based index.
jumpFocus ::
  Int {- ^ zero-based window index -} ->
  ClientState -> ClientState
jumpFocus i st
  | 0 <= i, i < Map.size windows = changeFocus focus st
  | otherwise                    = st
  where
    windows   = view clientWindows st
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
  . activateCurrent
  . deactivatePrevious
  . updatePrevious
  . set clientFocus focus
  . set clientSubfocus FocusMessages
  $ st
  where
    oldFocus = view clientFocus st

    updatePrevious
      | focus == oldFocus = id
      | otherwise         = set clientPrevFocus oldFocus

    -- always activate the new window. If it was already active this
    -- will clear the marker.
    activateCurrent = over (clientWindows . ix focus) windowActivate

    -- Don't deactivate a window if it's going to stay active
    deactivatePrevious
      | oldFocus `elem` focus : view clientExtraFocus st = id
      | otherwise = over (clientWindows . ix oldFocus) windowDeactivate


-- | Unified logic for assigning to the extra focuses field that activates
-- and deactivates windows as needed.
setExtraFocus :: [Focus] -> ClientState -> ClientState
setExtraFocus newFocuses st
  = aux windowDeactivate newlyInactive
  $ aux windowActivate   newlyActive
  $ set clientExtraFocus newFocuses st
  where
    newlyActive = newFocuses \\ (view clientFocus st : view clientExtraFocus st)

    newlyInactive = view clientExtraFocus st \\ (view clientFocus st : newFocuses)

    aux f xs st1 =
      foldl' (\acc w -> overStrict (clientWindows . ix w) f acc) st1 xs


-- | Change the subfocus to the given value, preserve the focus, reset
-- the scroll.
changeSubfocus ::
  Subfocus    {- ^ new subfocus -} ->
  ClientState {- ^ client state -} ->
  ClientState
changeSubfocus focus
  = set clientErrorMsg Nothing
  . set clientScroll 0
  . set clientSubfocus focus

-- | Return to previously focused window.
returnFocus :: ClientState -> ClientState
returnFocus st = changeFocus (view clientPrevFocus st) st

-- | Step focus to the next window when on message view. Otherwise
-- switch to message view.
advanceFocus :: ClientState -> ClientState
advanceFocus = stepFocus $ \l r ->
  fst . fst <$> Map.minViewWithKey r <|>
  fst . fst <$> Map.minViewWithKey l

-- | Step focus to the previous window when on message view. Otherwise
-- switch to message view.
retreatFocus :: ClientState -> ClientState
retreatFocus = stepFocus $ \l r ->
  fst . fst <$> Map.maxViewWithKey l <|>
  fst . fst <$> Map.maxViewWithKey r

-- | Step focus to the next window when on message view. Otherwise
-- switch to message view.
advanceNetworkFocus :: ClientState -> ClientState
advanceNetworkFocus = stepFocus $ \l r ->
  fst . fst <$> Map.minViewWithKey (Map.filterWithKey isNetwork r) <|>
  fst . fst <$> Map.minViewWithKey (Map.filterWithKey isNetwork l)
  where
    isNetwork k _ = has _NetworkFocus k

-- | Selection function used in 'stepFocus'
type FocusSelector =
  Map Focus Window {- ^ windows before current window -} ->
  Map Focus Window {- ^ windows after current window  -} ->
  Maybe Focus      {- ^ window to focus               -}

-- | Step focus to the next window when on message view. Otherwise
-- switch to message view. Reverse the step order when argument is 'True'.
stepFocus ::
  FocusSelector {- ^ selection function -} ->
  ClientState   {- ^ client state       -} ->
  ClientState
stepFocus selector st =
  case selector l r of
    Just k  -> changeFocus k st
    Nothing -> st
  where
    (l,r) = Map.split (view clientFocus st) (view clientWindows st)

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

-- | Returns the list of network names that requested autoconnection.
clientAutoconnects :: ClientState -> [Text]
clientAutoconnects st =
  [ network | (network, cfg) <- views (clientConfig . configServers) HashMap.toList st
            , view ssAutoconnect cfg
            ]

-- | Toggle the /hide metadata/ setting for the focused window.
clientToggleHideMeta :: ClientState -> ClientState
clientToggleHideMeta st =
  overStrict (clientWindows . ix (view clientFocus st) . winHideMeta) not st
