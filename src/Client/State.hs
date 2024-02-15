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
  , clientThreadJoins
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
  , clientNotifications
  , clientBell
  , clientUiFocused
  , clientExtensions
  , clientRegex
  , clientLogQueue
  , clientActivityReturn
  , clientErrorMsg
  , clientLayout
  , clientEditMode
  , clientEditLock
  , clientRtsStats
  , clientConfigPath
  , clientStsPolicy
  , clientHighlights

  -- * Client operations
  , withClientState
  , clientIsFiltered
  , clientFilter
  , clientFilterChannels
  , clientNetworkPalette
  , buildMatcher
  , clientToggleHideMeta
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
  , clientHighlightsFocus
  , clientWindowNames
  , clientPalette
  , clientAutoconnects
  , clientActiveCommand
  , clientNextWindowName
  , clientWindowHint
  , clientHelp

  , clientExtraFocuses
  , currentNickCompletionMode

  -- * Add messages to buffers
  , recordChannelMessage
  , recordNetworkMessage
  , recordError
  , recordIrcMessage
  , recordSuccess

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
  ) where

import           Client.CApi
import           Client.Commands.WordCompletion
import           Client.Configuration
import           Client.Configuration.ServerSettings
import           Client.Configuration.Sts
import           Client.Image.Message
import           Client.Image.PackedImage (imageText)
import           Client.Image.Palette
import           Client.Log
import           Client.Mask
import           Client.Message
import           Client.Network.Async
import           Client.State.Channel
import qualified Client.State.EditBox as Edit
import           Client.State.Focus
import           Client.State.Help
import           Client.State.Network
import           Client.State.Window
import           Client.State.Target (MessageTarget(..))
import           ContextFilter
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
import           Irc.Message hiding (MessageTarget(..))
import           Irc.RawIrcMsg
import           Irc.UserInfo
import           LensUtils
import           RtsStats (Stats)
import qualified System.Random as Random
import           Text.Regex.TDFA
import           Text.Regex.TDFA.String (compile)

-- | All state information for the IRC client
data ClientState = ClientState
  { _clientWindows           :: !(Map Focus Window) -- ^ client message buffers
  , _clientPrevFocus         :: !Focus              -- ^ previously focused buffer
  , _clientActivityReturn    :: !(Maybe Focus)      -- ^ focus prior to jumping to activity
  , _clientFocus             :: !Focus              -- ^ currently focused buffer
  , _clientSubfocus          :: !Subfocus           -- ^ current view mode
  , _clientExtraFocus        :: ![(Focus, Subfocus)]-- ^ extra messages windows to view

  , _clientConnections       :: !(HashMap Text NetworkState) -- ^ state of active connections
  , _clientEvents            :: !(TQueue NetworkEvent)    -- ^ incoming network event queue
  , _clientThreadJoins       :: TQueue (Int, ThreadEntry) -- ^ Finished threads ready to report

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
  , _clientLayout            :: LayoutMode                -- ^ layout mode for split screen
  , _clientEditMode          :: EditMode                  -- ^ editor rendering mode
  , _clientEditLock          :: Bool                      -- ^ editor locked and won't send

  , _clientNotifications     :: [(LText.Text, LText.Text)] -- ^ notifications to send next draw
  , _clientBell              :: !Bool                     -- ^ terminal bell on next redraw
  , _clientUiFocused         :: !Bool                     -- ^ whether the UI is focused; used by notifications

  , _clientIgnores           :: !(HashSet Identifier)     -- ^ ignored masks
  , _clientIgnoreMask        :: Mask                      -- ^ precomputed ignore regular expression (lazy)

  , _clientExtensions        :: !ExtensionState           -- ^ state of loaded extensions
  , _clientLogQueue          :: ![LogLine]                -- ^ log lines ready to write
  , _clientErrorMsg          :: Maybe Text                -- ^ transient error box text
  , _clientRtsStats          :: Maybe Stats               -- ^ most recent GHC RTS stats

  , _clientStsPolicy         :: !(HashMap Text StsPolicy) -- ^ STS policy entries
  , _clientHighlights        :: !(HashMap Identifier Highlight) -- ^ highlights
  , _clientHelp              :: !HelpState                -- ^ cached help text
  }

data Matcher = Matcher
  { matcherBefore :: !Int
  , matcherAfter  :: !Int
  , matcherMax    :: Maybe Int
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
     threadQueue <- atomically newTQueue
     sts       <- readPolicyFile
     let ignoreIds = map mkId (view configIgnores cfg)
     k ClientState
        { _clientWindows           = _Empty # ()
        , _clientIgnores           = HashSet.fromList ignoreIds
        , _clientIgnoreMask        = buildMask ignoreIds
        , _clientConnections       = _Empty # ()
        , _clientThreadJoins       = threadQueue
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
        , _clientEditMode          = SingleLineEditor
        , _clientEditLock          = False
        , _clientActivityBar       = view configActivityBar cfg
        , _clientShowPing          = view configShowPing cfg
        , _clientNotifications     = []
        , _clientBell              = False
        , _clientUiFocused         = True
        , _clientExtensions        = exts
        , _clientLogQueue          = []
        , _clientErrorMsg          = Nothing
        , _clientRtsStats          = Nothing
        , _clientStsPolicy         = sts
        , _clientHighlights        = HashMap.empty
        , _clientHelp              = makeHelp Nothing []
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

recordSuccess :: ZonedTime -> ClientState -> Text -> ClientState
recordSuccess now ste m =
  recordNetworkMessage ClientMessage
    { _msgTime    = now
    , _msgBody    = NormalBody m
    , _msgNetwork = ""
    } ste


-- | Add a message to the window associated with a given channel
recordChannelMessage ::
  Text       {- ^ network -} ->
  Identifier {- ^ channel -} ->
  ClientMessage ->
  ClientState ->
  ClientState
recordChannelMessage = recordChannelMessage' True

recordChannelMessage' ::
  Bool       {- ^ create  -} ->
  Text       {- ^ network -} ->
  Identifier {- ^ channel -} ->
  ClientMessage ->
  ClientState ->
  ClientState
recordChannelMessage' create network channel msg st
  = recordLogLine msg statusModes channel'
  $ recordWindowLine' create focus wl st
  where
    focus      = ChannelFocus network channel'
    wl         = toWindowLine rendParams importance msg

    rendParams = MessageRendererParams
      { rendStatusMsg   = statusModes
      , rendUserSigils  = computeMsgLineSigils network channel' msg st
      , rendHighlights  = highlights
      , rendPalette     = clientPalette st
      , rendAccounts    = accounts
      , rendNetPalette  = clientNetworkPalette st
      , rendChanTypes   = "#&!+" -- TODO: Don't hardcode this, use CHANTYPES ISUPPORT.
      }

    -- on failure returns mempty/""
    cs = st ^?! clientConnection network
    possibleStatusModes     = view csStatusMsg cs
    (statusModes, channel') = splitStatusMsgModes possibleStatusModes channel
    importance              = msgImportance msg st
    highlights              = clientHighlightsFocus (ChannelFocus network channel) st

    accounts =
      if view (csSettings . ssShowAccounts) cs
      then Just (view csUsers cs)
      else Nothing


recordLogLine ::
  ClientMessage {- ^ message      -} ->
  [Char]        {- ^ status modes -} ->
  Identifier    {- ^ target       -} ->
  ClientState   {- ^ client state -} ->
  ClientState
recordLogLine msg statusModes target st =
  case view (clientConnection (view msgNetwork msg) . csSettings . ssLogDir) st of
    Nothing -> st
    Just dir ->
      case renderLogLine msg dir statusModes target of
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
      highlights = clientHighlightsFocus (NetworkFocus network) st
      isMe x  = Just x == me
      checkTxt txt
        | any (\x -> Just HighlightMe == HashMap.lookup (mkId x) highlights)
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
        Wallops{} -> WLImportant
        Part who _ _ | isMe (userNick (srcUser who)) -> WLImportant
                     | otherwise -> WLBoring
        Kick _ _ kicked _ | isMe kicked -> WLImportant
                          | otherwise   -> WLNormal
        Error{} -> WLImportant

        -- away notices
        Reply _ RPL_AWAY _     -> WLBoring

        -- list output
        Reply _ RPL_LISTSTART _ -> WLBoring
        Reply _ RPL_LIST      _ -> WLBoring
        Reply _ RPL_LISTEND   _ -> WLBoring

        -- channel information
        Reply _ RPL_TOPIC _    -> WLBoring
        Reply _ RPL_INVITING _ -> WLBoring

        -- remaining replies go to network window
        Reply _ cmd _ ->
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
      | identIgnored (srcUser who) st = Just (userNick (srcUser who))
      | otherwise = Nothing



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
    TargetDrop         -> st
    TargetNetwork      -> recordNetworkMessage msg st
    TargetExisting win -> recordChannelMessage' False network win  msg st
    TargetWindow chan  -> recordChannelMessage' True  network chan msg st
    TargetUser user    -> recordUserMessage network user msg st

-- | Compute the sigils of the user who sent a message.
computeMsgLineSigils ::
  Text       {- ^ network -} ->
  Identifier {- ^ channel -} ->
  ClientMessage ->
  ClientState ->
  [Char] {- ^ sigils -}
computeMsgLineSigils network channel msg st =
  case msgActor =<< preview (msgBody . _IrcBody) msg of
    Just user -> computeUserSigils network channel (userNick (srcUser user)) st
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
    IrcBody (Reply _ code args)
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
    wl         = toWindowLine' network st importance msg

-- | Record a message on every window where a user is present.
recordUserMessage ::
  Text       {- ^ network -} ->
  Identifier {- ^ user -} ->
  ClientMessage ->
  ClientState ->
  ClientState
recordUserMessage network user msg st = foldl' foldFn st chans
  where
    -- FIXME: We discard the the boolean from addToWindow here,
    -- which means notifications for important cross-channel activity never happen.
    -- This currently affects nothing AFAIK, but who knows what the future holds?
    windowsLens chan = clientWindows . ix (ChannelFocus network chan)
    foldFn st' chan = overStrict (windowsLens chan) (fst . addToWindow wl) st'
    wl    = toWindowLine' network st WLBoring msg
    chans = user
          : case preview (clientConnection network . csChannels) st of
              Nothing -> []
              Just m  -> [chan | (chan, cs) <- HashMap.toList m
                               , HashMap.member user (view chanUsers cs) ]

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

clientNextWindowName :: Maybe WindowHint -> ClientState -> Char
clientNextWindowName hint st
  | Just n <- windowHintName =<< hint, n `notElem` usedNames = n
  | c:_ <- availableNames \\ usedNames    = c
  | otherwise                             = '\0'
  where
    usedNames = toListOf (clientWindows . folded . winName . _Just) st

    availableNames :: String
    availableNames = clientWindowNames st \\ reservedNames

    reservedNames :: String
    reservedNames =
      toListOf (clientConnections . folded . csSettings . ssWindowHints . folded . to windowHintName  . folded) st

clientWindowHint :: Focus -> ClientState -> Maybe WindowHint
clientWindowHint focus st =
 do net <- focusNetwork focus
    let hintFocus =
          case focus of
            Unfocused -> Unfocused
            NetworkFocus {} -> NetworkFocus ""
            ChannelFocus _ x -> ChannelFocus "" x
    preview (clientConnection net . csSettings . ssWindowHints . ix hintFocus) st

-- | Record window line at the given focus creating the window if necessary
recordWindowLine ::
  Focus ->
  WindowLine ->
  ClientState ->
  ClientState
recordWindowLine = recordWindowLine' True

recordWindowLine' ::
  Bool ->
  Focus ->
  WindowLine ->
  ClientState ->
  ClientState
recordWindowLine' create focus wl st = st1
  where
    hints = clientWindowHint focus st
    winActivity = fromMaybe AFLoud (windowHintActivity =<< hints)

    freshWindow = emptyWindow
      { _winName'    = clientNextWindowName hints st
      , _winHideMeta = fromMaybe (view (clientConfig . configHideMeta) st) (windowHintHideMeta =<< hints)
      , _winHidden   = fromMaybe False (windowHintHidden =<< hints)
      , _winActivityFilter   = winActivity
      }

    add True  w = Just $! addToWindow wl (fromMaybe freshWindow w)
    add False w = addToWindow wl <$> w

    addedMaybe = add create $ view (clientWindows . at focus) st
    st1 = case addedMaybe of
      Just (w', notify) -> addNotify notify focus wl $ set (clientWindows . at focus) (Just w') st
      Nothing -> st

addNotify :: Bool -> Focus -> WindowLine -> ClientState -> ClientState
addNotify False _     _  st = st
addNotify True  focus wl st
  | focus == view clientFocus st && view clientUiFocused st = st
  | otherwise = addBell $ over clientNotifications (cons (focusText focus, bodyText)) st
  where
    addBell st'
      | not (view clientBell st')
      , view (clientConfig . configBellOnMention) st' = set clientBell True st'
      | otherwise = st'
    bodyText = imageText (view wlPrefix wl) <> " " <> imageText (view wlImage wl)
    focusText Unfocused = "Application Notice"
    focusText (NetworkFocus net) = LText.fromChunks ["Notice from ", net]
    focusText (ChannelFocus net chan) = LText.fromChunks ["Activity on ", net, ":", idText chan]

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
toWindowLine' :: Text -> ClientState -> WindowLineImportance -> ClientMessage -> WindowLine
toWindowLine' network st =
  toWindowLine defaultRenderParams
    { rendPalette     = view (clientConfig . configPalette) st
    , rendHighlights  = clientHighlightsFocus (NetworkFocus network) st
    }


-- | Function applied to the client state every redraw.
clientTick :: ClientState -> ClientState
clientTick st = (if view clientUiFocused st then markSeen else id)
           . set clientBell False
           . set clientNotifications []
           . set clientLogQueue []
           $ st


-- | Mark the messages on the current window (and any splits) as seen.
markSeen :: ClientState -> ClientState
markSeen st = foldl' aux st messageFocuses
  where
    aux acc focus = overStrict (clientWindows . ix focus) windowSeen acc

    messageFocuses = [focus | (focus, FocusMessages) <- allFocuses]

    allFocuses = (view clientFocus st, view clientSubfocus st)
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
    -- Refer to the grep command in Client.Commands.Window
    Just ("grep" , reStr) -> buildMatcher reStr
    Just ("g" , reStr)    -> buildMatcher reStr
    _ -> case view clientRegex st of
           Nothing -> Nothing
           Just r  -> Just r

clientIsFiltered :: ClientState -> Bool
clientIsFiltered = isJust . clientMatcher

clientFilter :: ClientState -> (a -> LText.Text) -> [a] -> [a]
clientFilter st f xs =
  case clientMatcher st of
    Nothing -> xs
    Just m ->
      limit $
      filterContext
        (matcherAfter m) -- client messages are stored in descending order
        (matcherBefore m)
        (matcherPred m . f)
        xs
     where
       limit = maybe id take (matcherMax m)

clientFilterChannels ::
  ClientState ->
  Maybe Int ->
  Maybe Int ->
  [(Identifier, Int, Text)] ->
  [(Identifier, Int, Text)]
clientFilterChannels st min' (Just max') =
  filter (\(_, users, _) -> users < max') . clientFilterChannels st min' Nothing
clientFilterChannels st (Just min') Nothing =
  filter (\(_, users, _) -> users > min') . clientFilterChannels st Nothing Nothing
clientFilterChannels st Nothing Nothing = clientFilter st filterOn
  where filterOn (chan, _, topic) = LText.fromChunks [idText chan, " ", topic]

data MatcherArgs = MatcherArgs
  { argAfter     :: !Int
  , argBefore    :: !Int
  , argInvert    :: !Bool
  , argSensitive :: !Bool
  , argMax       :: Maybe Int
  , argPlain     :: !Bool
  }

defaultMatcherArgs :: MatcherArgs
defaultMatcherArgs = MatcherArgs
  { argAfter     = 0
  , argBefore    = 0
  , argInvert    = False
  , argSensitive = True
  , argMax       = Nothing
  , argPlain     = False
  }

buildMatcher :: String -> Maybe Matcher
buildMatcher = go defaultMatcherArgs
  where
    go !args reStr =
      case dropWhile (' '==) reStr of
        '-' : 'i' : ' ' : reStr' -> go args{argSensitive=False} reStr'
        '-' : 'v' : ' ' : reStr' -> go args{argInvert=True} reStr'
        '-' : 'F' : ' ' : reStr' -> go args{argPlain=True} reStr'
        '-' : 'A' : reStr' | [(a,' ':reStr'')] <- reads reStr', a>=0 -> go args{argAfter=a} reStr''
        '-' : 'B' : reStr' | [(b,' ':reStr'')] <- reads reStr', b>=0 -> go args{argBefore=b} reStr''
        '-' : 'C' : reStr' | [(c,' ':reStr'')] <- reads reStr', c>=0 -> go args{argAfter=c,argBefore=c} reStr''
        '-' : 'm' : reStr' | [(m,' ':reStr'')] <- reads reStr', m>=0 -> go args{argMax=Just m} reStr''
        '-' : '-' : ' ' : reStr' -> finish args reStr'
        _ -> finish args reStr

    finish args reStr
      | argPlain args =
          if argSensitive args
          then matcher (LText.isInfixOf (LText.fromStrict (Text.pack reStr)))
          else matcher (LText.isInfixOf (LText.fromStrict (Text.toLower (Text.pack reStr))) . LText.toLower)
      | otherwise =
        case compile defaultCompOpt{caseSensitive=argSensitive args}
                     defaultExecOpt{captureGroups=False}
                     reStr of
          Left{}  -> Nothing
          Right r -> matcher (matchTest r . LText.unpack)
      where
        matcher f
          | argInvert args = Just (Matcher (argBefore args) (argAfter args) (argMax args) (not . f))
          | otherwise      = Just (Matcher (argBefore args) (argAfter args) (argMax args) f)

-- | Compute the command and arguments currently in the textbox.
clientActiveCommand ::
  ClientState           {- ^ client state                     -} ->
  Maybe (String,String) {- ^ command name and argument string -}
clientActiveCommand st =
  case break (==' ') (dropWhile (' '==) (clientFirstLine st)) of
    ('/':cmd,_:args) -> Just (cmd,args)
    _                -> Nothing


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

     eSettings0 <-
       try $
       loadSecrets $
       fromMaybe defSettings $
       preview (clientConfig . configServers . ix network) st

     case eSettings0 of
       Left (SecretException label err) ->
         do now <- getZonedTime
            let txt = "Failed loading secret \x02" <> label <> "\x02: " <> err
            pure $! recordError now network (Text.pack txt) st

       Right settings0 ->
         do settings1 <- applyStsPolicy stsUpgrade settings0 st
            -- don't bother delaying on the first reconnect
            let delay = 15 * max 0 (attempts - 1)
            c <- createConnection delay settings1
            seed <- Random.newStdGen
            let restrict = case view ssTls settings1 of
                             TlsStart -> StartTLSRestriction
                             TlsYes   -> WaitTLSRestriction
                             TlsNo    -> NoRestriction
                cs = newNetworkState network settings1 c
                       (PingConnecting attempts lastTime restrict) seed
            traverse_ (sendMsg cs) (initialMessages cs)
            pure (set (clientConnections . at network) (Just cs) st)

applyStsPolicy :: Maybe Int -> ServerSettings -> ClientState -> IO ServerSettings
applyStsPolicy stsUpgrade settings st =
  do now <- getCurrentTime
     let stsUpgrade'
           | Just{} <- stsUpgrade = stsUpgrade
           | TlsNo <- view ssTls settings
           , let host = Text.pack (view ssHostName settings)
           , Just policy <- view (clientStsPolicy . at host) st
           , now < view stsExpiration policy
           = Just (view stsPort policy)
           | otherwise = Nothing
     pure $ case stsUpgrade' of
              Just port -> set ssPort (Just (fromIntegral port))
                         $ set ssTls TlsYes settings
              Nothing   -> settings

applyMessageToClientState ::
  ZonedTime                  {- ^ timestamp                -} ->
  IrcMsg                     {- ^ message received         -} ->
  Text                       {- ^ network name             -} ->
  NetworkState               {- ^ network connection state -} ->
  ClientState                {- ^ client state             -} ->
  ([RawIrcMsg], ClientState) {- ^ response , updated state -}
applyMessageToClientState time irc network cs st =
  cs' `seq` (reply, st')
  where
    Apply reply cs' = applyMessage time irc cs
    st' = applyWindowRenames network irc
        . applyHelpIfAwaiting network irc
        $ set (clientConnections . ix network) cs' st

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
    old' = userNick (srcUser old)

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

applyHelpIfAwaiting :: Text {- ^ network -} -> IrcMsg -> ClientState -> ClientState
applyHelpIfAwaiting network irc st
  | awaitingHelp (_clientHelp st) == Just network = over clientHelp (applyHelpReply (clientPalette st) irc) st
  | otherwise = st

------------------------------------------------------------------------
-- Scrolling
------------------------------------------------------------------------

-- | Scroll the current buffer to show newer messages
scrollClient :: Int -> ClientState -> ClientState
scrollClient amt = over clientScroll $ \n -> max 0 (n + amt)


-- | List of extra focuses to display as split windows
clientExtraFocuses :: ClientState -> [(Focus, Subfocus)]
clientExtraFocuses st =
  delete
    (view clientFocus st, view clientSubfocus st)
    (view clientExtraFocus st)

------------------------------------------------------------------------
-- Focus Management
------------------------------------------------------------------------

-- | Jump the focus of the client to a buffer that has unread activity.
-- Some events like errors or chat messages mentioning keywords are
-- considered important and will be jumped to first.
jumpToActivity :: ClientState -> ClientState
jumpToActivity st =
  case locate (Nothing, 1) windowList of
    Just focus -> changeFocus focus st
    Nothing ->
      case view clientActivityReturn st of
        Just focus -> changeFocus focus st
        Nothing    -> st
  where
    windowList = views clientWindows Map.toAscList st
    locate (v, _) [] = v
    locate vp@(_, vRank) ((f,w):wins)
      | fRank == 5 = Just f -- Short circuit
      | fRank > vRank = locate (Just f, fRank) wins
      | otherwise = locate vp wins
      where
        fRank = fromEnum (isJust $ view winName w) + 2 * fromEnum (view winMention w)

-- | Jump the focus directly to a window based on its zero-based index
-- while ignoring hidden windows.
jumpFocus ::
  Char {- ^ window name -} ->
  ClientState -> ClientState
jumpFocus i st =
  case find p (Map.assocs (view clientWindows st)) of
    Nothing        -> st
    Just (focus,_) -> changeFocus focus st
  where
    p (_, w) = view winName w == Just i


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
      | (oldFocus, FocusMessages) `elem` (focus, FocusMessages) : view clientExtraFocus st = id
      | otherwise = over (clientWindows . ix oldFocus) windowDeactivate


-- | Unified logic for assigning to the extra focuses field that activates
-- and deactivates windows as needed.
setExtraFocus :: [(Focus, Subfocus)] -> ClientState -> ClientState
setExtraFocus newFocuses st
  = aux windowDeactivate newlyInactive
  $ aux windowActivate   newlyActive
  $ set clientExtraFocus newFocuses st
  where
    messagePart x = [focus | (focus, FocusMessages) <- x]

    current = (view clientFocus st, view clientSubfocus st)

    newlyActive = messagePart newFocuses \\ messagePart (current : view clientExtraFocus st)

    newlyInactive = messagePart (view clientExtraFocus st)
                 \\ messagePart (current : newFocuses)

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
    (l,r) = Map.split (view clientFocus st)
          $ Map.filter (views winHidden not)
          $ view clientWindows st

clientHighlightsFocus ::
  Focus ->
  ClientState ->
  HashMap Identifier Highlight
clientHighlightsFocus focus st =
  case focus of
    ChannelFocus n c -> netcase n (Just c)
    NetworkFocus n   -> netcase n Nothing
    Unfocused        -> base
  where
    base = HashMap.fromList [(x, HighlightMe) | x <- view (clientConfig . configExtraHighlights) st]
        <> HashMap.fromList [(x, HighlightNone) | x <- view (clientConfig . configNeverHighlights) st]
        <> view clientHighlights st

    replace x y =
      case x of
        HighlightError -> y
        _              -> x

    netcase n mbC =
      case preview (clientConnection n) st of
        Nothing -> view clientHighlights st
        Just cs ->
          HashMap.unionWith
            replace
            (HashMap.insert (view csNick cs) HighlightMe base)
            (HashMap.fromList [(u, HighlightNick)
                                | Just c <- [mbC]
                                , u <- views (csChannels . ix c . chanUsers) HashMap.keys cs
                                , Text.length (idText u) > 1 ])

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

-- | Generates the NetworkPalette for the current focus.
clientNetworkPalette :: ClientState -> NetworkPalette
clientNetworkPalette st = case focusNetwork (view clientFocus st) of
  Just net -> configNetworkPalette net (view clientConfig st)
  Nothing  -> defaultNetworkPalette
