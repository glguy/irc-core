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
  , clientDCC
  , clientDCCUpdates
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
  , clientShowPing
  , clientSubfocus
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
  , clientStsPolicy

  -- * Client operations
  , withClientState
  , clientMatcher, Matcher(..), buildMatcher
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
  , queueDCCTransfer
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
  , recordError
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
  , esMVar
  , esStablePtr

  -- * URL view
  , urlPattern
  , urlMatches

  ) where

import           Client.CApi
import           Client.Commands.WordCompletion
import           Client.Configuration
import           Client.Configuration.ServerSettings
import           Client.Configuration.Sts
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
import           Client.State.DCC
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Foldable
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
  , _clientActivityReturn    :: !(Maybe Focus)      -- ^ focus prior to jumping to activity
  , _clientFocus             :: !Focus              -- ^ currently focused buffer
  , _clientSubfocus          :: !Subfocus           -- ^ current view mode
  , _clientExtraFocus        :: ![Focus]            -- ^ extra messages windows to view

  , _clientConnections       :: !(HashMap Text NetworkState) -- ^ state of active connections
  , _clientEvents            :: !(TQueue NetworkEvent)    -- ^ incoming network event queue
  , _clientDCC               :: !DCCState                 -- ^ DCC subsystem
  , _clientDCCUpdates        :: !(TChan DCCUpdate)        -- ^ DCC update events

  , _clientConfig            :: !Configuration            -- ^ client configuration
  , _clientConfigPath        :: !FilePath                 -- ^ client configuration file path

  , _clientTextBox           :: !Edit.EditBox             -- ^ primary text box
  , _clientTextBoxOffset     :: !Int                      -- ^ size to crop from left of text box
  , _clientWidth             :: !Int                      -- ^ current terminal width
  , _clientHeight            :: !Int                      -- ^ current terminal height

  , _clientScroll            :: !Int                      -- ^ buffer scroll lines
  , _clientDetailView        :: !Bool                     -- ^ use detailed rendering mode
  , _clientActivityBar       :: !Bool                     -- ^ visible activity bar
  , _clientShowPing          :: !Bool                     -- ^ visible ping time
  , _clientRegex             :: Maybe Matcher             -- ^ optional persistent filter
  , _clientLayout            :: !LayoutMode               -- ^ layout mode for split screen

  , _clientBell              :: !Bool                     -- ^ sound a bell next draw

  , _clientIgnores           :: !(HashSet Identifier)     -- ^ ignored masks
  , _clientIgnoreMask        :: Mask                      -- ^ precomputed ignore regular expression (lazy)

  , _clientExtensions        :: !ExtensionState           -- ^ state of loaded extensions
  , _clientLogQueue          :: ![LogLine]                -- ^ log lines ready to write
  , _clientErrorMsg          :: Maybe Text                -- ^ transient error box text
  , _clientRtsStats          :: Maybe Stats               -- ^ most recent GHC RTS stats

  , _clientStsPolicy         :: !(HashMap Text StsPolicy) -- ^ STS policy entries
  }

data Matcher = Matcher
  { matcherBefore :: !Int
  , matcherAfter  :: !Int
  , matcherPred   :: LText.Text -> Bool
  }

-- | State of the extension API including loaded extensions and the mechanism used
-- to support reentry into the Haskell runtime from the C API.
--
-- When executing inside an extension the mvar will contain the client state
-- and the ID of the running extension.
data ExtensionState = ExtensionState
  { _esActive    :: IntMap ActiveExtension     -- ^ active extensions
  , _esMVar      :: MVar ParkState             -- ^ 'MVar' used to with 'clientPark'
  , _esStablePtr :: StablePtr (MVar ParkState) -- ^ 'StablePtr' used with 'clientPark'
  }

-- | ID of active extension and stored client state
type ParkState = (Int,ClientState)

makeLenses ''ClientState
makeLenses ''ExtensionState

-- | 'Traversal' for finding the 'NetworkState' associated with a given network
-- if that connection is currently active.
clientConnection ::
  Applicative f =>
  Text {- ^ network -} ->
  LensLike' f ClientState NetworkState
clientConnection network = clientConnections . ix network

-- | The full top-most line that would be executed
clientFirstLine :: ClientState -> String
clientFirstLine = fst . Edit.shift . view (clientTextBox . Edit.content)

-- | The line under the cursor in the edit box.
clientLine :: ClientState -> (Int, String) {- ^ line number, line content -}
clientLine = views (clientTextBox . Edit.line) (\(Edit.Line n t) -> (n, t))

-- | Construct an initial 'ClientState' using default values.
withClientState :: FilePath -> Configuration -> (ClientState -> IO a) -> IO a
withClientState cfgPath cfg k =

  withExtensionState $ \exts ->

  do events    <- atomically newTQueue
     dccEvents <- atomically newTChan
     sts       <- readPolicyFile
     let ignoreIds = map mkId (view configIgnores cfg)
     k ClientState
        { _clientWindows           = _Empty # ()
        , _clientIgnores           = HashSet.fromList ignoreIds
        , _clientIgnoreMask        = buildMask ignoreIds
        , _clientConnections       = _Empty # ()
        , _clientDCC               = emptyDCCState
        , _clientDCCUpdates        = dccEvents
        , _clientTextBox           = Edit.defaultEditBox
        , _clientTextBoxOffset     = 0
        , _clientWidth             = 80
        , _clientHeight            = 25
        , _clientEvents            = events
        , _clientPrevFocus         = Unfocused
        , _clientActivityReturn    = Nothing
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
        , _clientShowPing          = view configShowPing cfg
        , _clientBell              = False
        , _clientExtensions        = exts
        , _clientLogQueue          = []
        , _clientErrorMsg          = Nothing
        , _clientRtsStats          = Nothing
        , _clientStsPolicy         = sts
        }

withExtensionState :: (ExtensionState -> IO a) -> IO a
withExtensionState k =
  do mvar <- newEmptyMVar
     bracket (newStablePtr mvar) freeStablePtr $ \stab ->
       k ExtensionState
         { _esActive    = IntMap.empty
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
                  return $! over clientConnections (sans network) st


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

        -- channel information
        Reply RPL_TOPIC _        -> WLBoring
        Reply RPL_INVITING _     -> WLBoring

        -- remaining replies go to network window
        Reply cmd _ ->
          case replyCodeType (replyCodeInfo cmd) of
            ErrorReply -> WLImportant
            _          -> WLNormal
        _              -> WLBoring


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

recordError ::
  ZonedTime       {- ^ now             -} ->
  Text            {- ^ network         -} ->
  Text            {- ^ error message   -} ->
  ClientState     {- ^ client state    -} ->
  ClientState
recordError now net msg =
  recordNetworkMessage ClientMessage
    { _msgTime    = now
    , _msgNetwork = net
    , _msgBody    = ErrorBody msg
    }

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
  ClientState   {- ^ client state       -} ->
  Maybe Matcher {- ^ optional predicate -}
clientMatcher st =
  case clientActiveCommand st of
    Just ("grep" , reStr) -> buildMatcher reStr
    _ -> case view clientRegex st of
           Nothing -> Nothing
           Just r  -> Just r

buildMatcher :: String -> Maybe Matcher
buildMatcher = go (True, 0, 0)
  where
    go (sensitive, b, a) reStr =
      case dropWhile (' '==) reStr of
        '-' : 'i' : ' ' : reStr'                                            -> go (False, b, a) reStr'
        '-' : 'A' : reStr' | [(a' , ' ':reStr'')] <- reads reStr', a'  >= 0 -> go (sensitive, b, a') reStr''
        '-' : 'B' : reStr' | [(b' , ' ':reStr'')] <- reads reStr', b'  >= 0 -> go (sensitive, b', a) reStr''
        '-' : 'C' : reStr' | [(num, ' ':reStr'')] <- reads reStr', num >= 0 -> go (sensitive, num, num) reStr''
        '-' : '-' : reStr' -> finish (sensitive, b, a) (drop 1 reStr')
        _ -> finish (sensitive, b, a) reStr

    finish (sensitive, b, a) reStr =
      case compile defaultCompOpt{caseSensitive=sensitive}
                   defaultExecOpt{captureGroups=False}
                   reStr of
        Left{}  -> Nothing
        Right r -> Just (Matcher b a (matchTest r . LText.unpack))

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
    "https?://([[:alnum:]-]+\\.)*([[:alnum:]-]+)(:[[:digit:]]+)?(/[-0-9a-zA-Z$_.+!*'(),%?=:@/;~#]*)?|\
    \<https?://[^>]*>|\
    \\\(https?://[^\\)]*\\)"


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
       Just ('(',t') | not (Text.null t') -> Text.init t'
       _                                  -> t

-- | Remove a network connection and unlink it from the network map.
-- This operation assumes that the network connection exists and should
-- only be applied once per connection.
removeNetwork :: Text -> ClientState -> (NetworkState, ClientState)
removeNetwork network st =
  case (clientConnections . at network <<.~ Nothing) st of
    (Nothing, _  ) -> error "removeNetwork: network not found"
    (Just cs, st1) -> (cs, st1)

-- | Start a new connection. The delay is used for reconnections.
addConnection ::
  Int           {- ^ attempts                 -} ->
  Maybe UTCTime {- ^ optional disconnect time -} ->
  Maybe Int     {- ^ STS upgrade port         -} ->
  Text          {- ^ network name             -} ->
  ClientState ->
  IO ClientState
addConnection attempts lastTime stsUpgrade network st =
  do let defSettings = (view (clientConfig . configDefaults) st)
                     { _ssName = Just network
                     , _ssHostName = Text.unpack network
                     }

         settings = fromMaybe defSettings
                  $ preview (clientConfig . configServers . ix network) st

     now <- getCurrentTime
     let stsUpgrade'
           | Just{} <- stsUpgrade = stsUpgrade
           | UseInsecure <- view ssTls settings
           , let host = Text.pack (view ssHostName settings)
           , Just policy <- view (clientStsPolicy . at host) st
           , now < view stsExpiration policy
           = Just (view stsPort policy)
           | otherwise = Nothing

         settings1 =
           case stsUpgrade' of
             Just port -> set ssPort (Just (fromIntegral port))
                        $ set ssTls UseTls settings
             Nothing   -> settings


         -- don't bother delaying on the first reconnect
         delay = 15 * max 0 (attempts - 1)

     c <- createConnection
            delay
            settings1

     let cs = newNetworkState network settings1 c (PingConnecting attempts lastTime)
     traverse_ (sendMsg cs) (initialMessages cs)

     return $ set (clientConnections . at network) (Just cs) st


applyMessageToClientState ::
  ZonedTime                  {- ^ timestamp                -} ->
  IrcMsg                     {- ^ message received         -} ->
  Text                       {- ^ network name             -} ->
  NetworkState               {- ^ network connection state -} ->
  ClientState                {- ^ client state             -} ->
  ([RawIrcMsg], Maybe DCCUpdate, ClientState) {- ^ response , DCC updates, updated state -}
applyMessageToClientState time irc network cs st =
  cs' `seq` (reply, dccUp, st')
  where
    (reply, cs') = applyMessage time irc cs
    (st', dccUp) = queueDCCTransfer network irc
                 $ applyWindowRenames network irc
                 $ set (clientConnections . ix network) cs' st

-- | Queue a DCC transfer when the message is correct. Await for user
--   confirmation to start the download.
queueDCCTransfer :: Text -> IrcMsg -> ClientState
                 -> (ClientState, Maybe DCCUpdate)
queueDCCTransfer network ctcpMsg st
  | Just (fromU, _target, command, txt) <- ctcpToTuple ctcpMsg
  , command == "DCC", dccState <- view clientDCC st
  = case (parseSEND network fromU txt, parseACCEPT dccState fromU txt) of
      (Right offer, _) -> (set clientDCC (insertAsNewMax offer dccState) st, Nothing)
      (_, Just upd) -> (st, Just upd)
      (_, _) -> (st, Nothing)

  | otherwise = (st, Nothing)


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
jumpToActivity st =
  case mplus highPriority lowPriority of
    Just (focus,_) -> changeFocus focus st
    Nothing ->
      case view clientActivityReturn st of
        Just focus -> changeFocus focus st
        Nothing    -> st
  where
    windowList   = views clientWindows Map.toAscList st
    highPriority = find (\x -> WLImportant == view winMention (snd x)) windowList
    lowPriority  = find (\x -> view winUnread (snd x) > 0) windowList

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
