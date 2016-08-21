{-# LANGUAGE BangPatterns, OverloadedStrings #-}

{-|
Module      : Client.Commands
Description : Implementation of slash commands
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module renders the lines used in the channel mask list. A mask list
can show channel bans, quiets, invites, and exceptions.
-}

module Client.Commands
  ( CommandResult(..)
  , execute
  , executeUserCommand
  , tabCompletion
  ) where

import           Client.CApi
import           Client.Commands.Interpolation
import           Client.Commands.WordCompletion
import           Client.Configuration
import           Client.Configuration.ServerSettings
import           Client.Message
import           Client.State
import           Client.State.Channel
import qualified Client.State.EditBox as Edit
import           Client.State.Focus
import           Client.State.Network
import           Client.State.Window
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import           Data.HashSet (HashSet)
import           Data.List.Split
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Monoid ((<>))
import           Data.Time
import           Irc.Commands
import           Irc.Identifier
import           Irc.RawIrcMsg
import           Irc.Message
import           Irc.UserInfo
import           Irc.Modes
import           LensUtils

-- | Possible results of running a command
data CommandResult
  = CommandSuccess ClientState
    -- ^ Continue running the client, consume input if command was from input
  | CommandFailure ClientState
    -- ^ Continue running the client, report an error
  | CommandQuit ClientState -- ^ Client should close

-- | Type of commands that always work
type ClientCommand =
  ClientState                                      ->
  String          {- ^ command arguments        -} ->
  IO CommandResult

-- | Type of commands that require an active network to be focused
type NetworkCommand =
  Text            {- ^ focused network          -} ->
  NetworkState    {- ^ focused connection state -} ->
  ClientState                                      ->
  String          {- ^ command arguments        -} ->
  IO CommandResult

-- | Type of commands that require an active channel to be focused
type ChannelCommand =
  Text            {- ^ focused network          -} ->
  NetworkState    {- ^ focused connection state -} ->
  Identifier      {- ^ focused channel          -} ->
  ClientState                                      ->
  String          {- ^ command arguments        -} ->
  IO CommandResult

-- | Pair of implementations for executing a command and tab completing one.
-- The tab-completion logic is extended with a bool
-- indicating that tab completion should be reversed
data Command
  = ClientCommand  ClientCommand  (Bool -> ClientCommand) -- ^ no requirements
  | NetworkCommand NetworkCommand (Bool -> NetworkCommand) -- ^ requires an active network
  | ChatCommand    ChannelCommand (Bool -> ChannelCommand) -- ^ requires an active chat window
  | ChannelCommand ChannelCommand (Bool -> ChannelCommand) -- ^ requires an active channel window

-- | Consider the text entry successful and resume the client
commandSuccess :: Monad m => ClientState -> m CommandResult
commandSuccess = return . CommandSuccess

-- | Consider the text entry a failure and resume the client
commandFailure :: Monad m => ClientState -> m CommandResult
commandFailure = return . CommandFailure

-- | Interpret the given chat message or command. Leading @/@ indicates a
-- command. Otherwise if a channel or user query is focused a chat message
-- will be sent.
execute ::
  String {- ^ chat or command -} ->
  ClientState -> IO CommandResult
execute str st =
  case str of
    []          -> commandFailure st
    '/':command -> executeUserCommand command st
    msg         -> executeChat msg st

-- | Execute command provided by user, resolve aliases if necessary.
executeUserCommand :: String -> ClientState -> IO CommandResult
executeUserCommand command st =
  let key = Text.pack (takeWhile (/=' ') command) in

  case preview (clientConfig . configMacros . ix key) st of
    Nothing -> executeCommand Nothing command st
    Just cmdExs ->
      case traverse (resolveExpansions expandVar expandInt) cmdExs of
        Nothing   -> commandFailure st
        Just cmds -> process cmds st
  where
    args = Text.words (Text.pack command)

    expandInt i = preview (ix (fromInteger i)) args

    expandVar v =
      case v of
        "network" -> views clientFocus focusNetwork st
        "channel" -> previews (clientFocus . _ChannelFocus . _2) idText st
        "nick"    -> do net <- views clientFocus focusNetwork st
                        cs  <- preview (clientConnection net) st
                        return (views csNick idText cs)
        _         -> Nothing

    process [] st0 = commandSuccess st0
    process (c:cs) st0 =
      do res <- executeCommand Nothing (Text.unpack c) st0
         case res of
           CommandSuccess st1 -> process cs st1
           CommandFailure st1 -> process cs st1 -- ?
           CommandQuit st1    -> return (CommandQuit st1)

-- | Respond to the TAB key being pressed. This can dispatch to a command
-- specific completion mode when relevant. Otherwise this will complete
-- input based on the users of the channel related to the current buffer.
tabCompletion :: Bool {- ^ reversed -} -> ClientState -> IO CommandResult
tabCompletion isReversed st =
  case snd $ clientLine st of
    '/':command -> executeCommand (Just isReversed) command st
    _           -> commandSuccess (nickTabCompletion isReversed st)

-- | Treat the current text input as a chat message and send it.
executeChat :: String -> ClientState -> IO CommandResult
executeChat msg st =
  case view clientFocus st of
    ChannelFocus network channel
      | Just !cs <- preview (clientConnection network) st ->
          do now <- getZonedTime
             let msgTxt = Text.pack $ takeWhile (/='\n') msg
                 ircMsg = ircPrivmsg channel msgTxt
                 myNick = UserInfo (view csNick cs) "" ""
                 entry = ClientMessage
                            { _msgTime    = now
                            , _msgNetwork = network
                            , _msgBody    = IrcBody (Privmsg myNick channel msgTxt)
                            }
             sendMsg cs ircMsg
             commandSuccess $ recordChannelMessage network channel entry st

    _ -> commandFailure st

splitWord :: String -> (String, String)
splitWord str = (w, drop 1 rest)
  where
    (w, rest) = break isSpace str

nextWord :: String -> Maybe (String, String)
nextWord str =
  case splitWord (dropWhile isSpace str) of
    (a,b) | null a    -> Nothing
          | otherwise -> Just (a,b)

-- | Parse and execute the given command. When the first argument is Nothing
-- the command is executed, otherwise the first argument is the cursor
-- position for tab-completion
executeCommand :: Maybe Bool -> String -> ClientState -> IO CommandResult

executeCommand (Just isReversed) _ st
  | Just st' <- commandNameCompletion isReversed st = commandSuccess st'

executeCommand tabCompleteReversed str st =
  let (cmd, rest) = splitWord str
      cmdTxt      = Text.toLower (Text.pack cmd) in
  case HashMap.lookup cmdTxt commands of

    Just (ClientCommand exec tab) ->
          maybe exec tab tabCompleteReversed
            st rest

    Just (NetworkCommand exec tab)
      | Just network <- views clientFocus focusNetwork st
      , Just cs      <- preview (clientConnection network) st ->
          maybe exec tab tabCompleteReversed
            network cs st rest

    Just (ChannelCommand exec tab)
      | ChannelFocus network channelId <- view clientFocus st
      , Just cs <- preview (clientConnection network) st
      , isChannelIdentifier cs channelId ->
          maybe exec tab tabCompleteReversed
            network cs channelId st rest

    Just (ChatCommand exec tab)
      | ChannelFocus network channelId <- view clientFocus st
      , Just cs <- preview (clientConnection network) st ->
          maybe exec tab tabCompleteReversed
            network cs channelId st rest

    _ -> case tabCompleteReversed of
           Nothing         -> commandFailure st
           Just isReversed -> commandSuccess (nickTabCompletion isReversed st)

-- Expands each alias to have its own copy of the command callbacks
expandAliases :: [([a],b)] -> [(a,b)]
expandAliases xs = [ (a,b) | (as,b) <- xs, a <- as ]

commands :: HashMap Text Command
commands = HashMap.fromList
         $ expandAliases
  [ (["connect"   ], ClientCommand cmdConnect noClientTab)
  , (["exit"      ], ClientCommand cmdExit    noClientTab)
  , (["focus"     ], ClientCommand cmdFocus   tabFocus)
  , (["clear"     ], ClientCommand cmdClear   noClientTab)
  , (["reconnect" ], ClientCommand cmdReconnect noClientTab)
  , (["ignore"    ], ClientCommand cmdIgnore simpleClientTab)
  , (["reload"    ], ClientCommand cmdReload  tabReload)
  , (["extension" ], ClientCommand cmdExtension simpleClientTab)

  , (["quote"     ], NetworkCommand cmdQuote  simpleNetworkTab)
  , (["j","join"  ], NetworkCommand cmdJoin   simpleNetworkTab)
  , (["c","channel"], NetworkCommand cmdChannel simpleNetworkTab)
  , (["mode"      ], NetworkCommand cmdMode   tabMode)
  , (["msg"       ], NetworkCommand cmdMsg    simpleNetworkTab)
  , (["notice"    ], NetworkCommand cmdNotice simpleNetworkTab)
  , (["ctcp"      ], NetworkCommand cmdCtcp   simpleNetworkTab)
  , (["nick"      ], NetworkCommand cmdNick   simpleNetworkTab)
  , (["quit"      ], NetworkCommand cmdQuit   simpleNetworkTab)
  , (["disconnect"], NetworkCommand cmdDisconnect noNetworkTab)
  , (["who"       ], NetworkCommand cmdWho    simpleNetworkTab)
  , (["whois"     ], NetworkCommand cmdWhois  simpleNetworkTab)
  , (["whowas"    ], NetworkCommand cmdWhowas simpleNetworkTab)
  , (["ison"      ], NetworkCommand cmdIson   simpleNetworkTab)
  , (["userhost"  ], NetworkCommand cmdUserhost simpleNetworkTab)
  , (["away"      ], NetworkCommand cmdAway   simpleNetworkTab)
  , (["links"     ], NetworkCommand cmdLinks  simpleNetworkTab)
  , (["time"      ], NetworkCommand cmdTime   simpleNetworkTab)
  , (["stats"     ], NetworkCommand cmdStats  simpleNetworkTab)
  , (["znc"       ], NetworkCommand cmdZnc    simpleNetworkTab)
  , (["znc-playback"], NetworkCommand cmdZncPlayback noNetworkTab)

  , (["invite"    ], ChannelCommand cmdInvite simpleChannelTab)
  , (["topic"     ], ChannelCommand cmdTopic  tabTopic    )
  , (["kick"      ], ChannelCommand cmdKick   simpleChannelTab)
  , (["kickban"   ], ChannelCommand cmdKickBan simpleChannelTab)
  , (["remove"    ], ChannelCommand cmdRemove simpleChannelTab)
  , (["part"      ], ChannelCommand cmdPart   simpleChannelTab)

  , (["users"     ], ChannelCommand cmdUsers  noChannelTab)
  , (["channelinfo"], ChannelCommand cmdChannelInfo noChannelTab)
  , (["masks"     ], ChannelCommand cmdMasks  noChannelTab)

  , (["me"        ], ChatCommand cmdMe     simpleChannelTab)
  , (["say"       ], ChatCommand cmdSay    simpleChannelTab)
  ]

noClientTab :: Bool -> ClientCommand
noClientTab _ st _ = commandFailure st

noNetworkTab :: Bool -> NetworkCommand
noNetworkTab _ _ _ st _ = commandFailure st

noChannelTab :: Bool -> ChannelCommand
noChannelTab _ _ _ _ st _ = commandFailure st

simpleClientTab :: Bool -> ClientCommand
simpleClientTab isReversed st _ =
  commandSuccess (nickTabCompletion isReversed st)

simpleNetworkTab :: Bool -> NetworkCommand
simpleNetworkTab isReversed _ _ st _ =
  commandSuccess (nickTabCompletion isReversed st)

simpleChannelTab :: Bool -> ChannelCommand
simpleChannelTab isReversed _ _ _ st _ =
  commandSuccess (nickTabCompletion isReversed st)

cmdExit :: ClientCommand
cmdExit st _ = return (CommandQuit st)

-- | When used on a channel that the user is currently
-- joined to this command will clear the messages but
-- preserve the window. When used on a window that the
-- user is not joined to this command will delete the window.
cmdClear :: ClientCommand
cmdClear st _ = commandSuccess (windowEffect st)
  where
    windowEffect
      | isActive  = clearWindow
      | otherwise = deleteWindow

    deleteWindow = advanceFocus . setWindow Nothing
    clearWindow  =                setWindow (Just emptyWindow)

    setWindow = set (clientWindows . at (view clientFocus st))

    isActive =
      case view clientFocus st of
        Unfocused -> False
        NetworkFocus network ->
            has (clientConnection network) st
        ChannelFocus network channel ->
            has ( clientConnection network
                . csChannels . ix channel) st


cmdQuote :: NetworkCommand
cmdQuote _ cs st rest =
  case parseRawIrcMsg (Text.pack rest) of
    Nothing  -> commandFailure st
    Just raw ->
      do sendMsg cs raw
         commandSuccess st

-- | Implementation of @/me@
cmdMe :: ChannelCommand
cmdMe network cs channelId st rest =
  do now <- getZonedTime
     let actionTxt = Text.pack ("\^AACTION " ++ rest ++ "\^A")
         !myNick = UserInfo (view csNick cs) "" ""
         entry = ClientMessage
                    { _msgTime = now
                    , _msgNetwork = network
                    , _msgBody = IrcBody (Ctcp myNick channelId "ACTION" (Text.pack rest))
                    }
     sendMsg cs (ircPrivmsg channelId actionTxt)
     commandSuccess
       $ recordChannelMessage network channelId entry st

-- | Implementation of @/ctcp@
cmdCtcp :: NetworkCommand
cmdCtcp network cs st rest =
  case parse of
    Nothing -> commandFailure st
    Just (target, cmd, args) ->
      do let cmdTxt = Text.toUpper (Text.pack cmd)
             argTxt = Text.pack args
             tgtTxt = Text.pack target

         sendMsg cs (ircPrivmsg (mkId tgtTxt) ("\^A" <> cmdTxt <> " " <> argTxt <> "\^A"))
         chatCommand
            (\src tgt -> Ctcp src tgt cmdTxt argTxt)
            tgtTxt
            network cs st
  where
    parse =
      do (target, rest1) <- nextWord rest
         (cmd   , args ) <- nextWord rest1
         return (target, cmd, args)

-- | Implementation of @/notice@
cmdNotice :: NetworkCommand
cmdNotice network cs st rest =
  case nextWord rest of
    Nothing -> commandFailure st
    Just (target, rest1) ->
      do let restTxt = Text.pack rest1
             tgtTxt = Text.pack target

         sendMsg cs (ircNotice (mkId tgtTxt) restTxt)
         chatCommand
            (\src tgt -> Notice src tgt restTxt)
            tgtTxt
            network cs st

-- | Implementation of @/msg@
cmdMsg :: NetworkCommand
cmdMsg network cs st rest =
  case nextWord rest of
    Nothing -> commandFailure st
    Just (target, rest1) ->
      do let restTxt = Text.pack rest1
             tgtTxt = Text.pack target

         sendMsg cs (ircPrivmsg (mkId tgtTxt) restTxt)
         chatCommand
            (\src tgt -> Privmsg src tgt restTxt)
            tgtTxt
            network cs st

-- | Common logic for @/msg@ and @/notice@
chatCommand ::
  (UserInfo -> Identifier -> IrcMsg) ->
  Text {- ^ target  -} ->
  Text {- ^ network -} ->
  NetworkState         ->
  ClientState          ->
  IO CommandResult
chatCommand con targetsTxt network cs st =
  do now <- getZonedTime
     let targetTxts = Text.split (==',') targetsTxt
         targetIds  = mkId <$> targetTxts
         !myNick = UserInfo (view csNick cs) "" ""
         entries = [ (targetId,
                          ClientMessage
                          { _msgTime = now
                          , _msgNetwork = network
                          , _msgBody = IrcBody (con myNick targetId)
                          })
                       | targetId <- targetIds ]

         st' = foldl' (\acc (targetId, entry) ->
                             recordChannelMessage network targetId entry acc)
                          st
                          entries

     commandSuccess st'

cmdConnect :: ClientCommand
cmdConnect st rest =
  case words rest of
    [networkStr] ->
      do -- abort any existing connection before connecting
         let network = Text.pack networkStr
         st' <- addConnection network =<< abortNetwork network st
         commandSuccess
           $ changeFocus (NetworkFocus network) st'

    _ -> commandFailure st

cmdFocus :: ClientCommand
cmdFocus st rest =
  case words rest of
    [network] ->
      let focus = NetworkFocus (Text.pack network) in
      commandSuccess (changeFocus focus st)

    [network,channel] ->
      let focus = ChannelFocus (Text.pack network) (mkId (Text.pack channel)) in
      commandSuccess
        $ changeFocus focus st

    _ -> commandFailure st

-- | When tab completing the first parameter of the focus command
-- the current networks are used.
tabFocus :: Bool -> ClientCommand
tabFocus isReversed st _
  = commandSuccess
  $ fromMaybe st
  $ clientTextBox (wordComplete id isReversed [] completions) st
  where
    networks   = map mkId $ HashMap.keys $ view clientNetworkMap st
    params     = words $ uncurry take $ clientLine st

    completions
      | length params == 2 = networks
      | otherwise          = currentCompletionList st

cmdWhois :: NetworkCommand
cmdWhois _ cs st rest =
  do sendMsg cs (ircWhois (Text.pack <$> words rest))
     commandSuccess st

cmdWho :: NetworkCommand
cmdWho _ cs st rest =
  do sendMsg cs (ircWho (Text.pack <$> words rest))
     commandSuccess st

cmdWhowas :: NetworkCommand
cmdWhowas _ cs st rest =
  do sendMsg cs (ircWhowas (Text.pack <$> words rest))
     commandSuccess st

cmdIson :: NetworkCommand
cmdIson _ cs st rest =
  do sendMsg cs (ircIson (Text.pack <$> words rest))
     commandSuccess st

cmdUserhost :: NetworkCommand
cmdUserhost _ cs st rest =
  do sendMsg cs (ircUserhost (Text.pack <$> words rest))
     commandSuccess st

cmdStats :: NetworkCommand
cmdStats _ cs st rest =
  do sendMsg cs (ircStats (Text.pack <$> words rest))
     commandSuccess st

cmdAway :: NetworkCommand
cmdAway _ cs st rest =
  do sendMsg cs (ircAway (Text.pack (dropWhile (==' ') rest)))
     commandSuccess st

cmdLinks :: NetworkCommand
cmdLinks _ cs st rest =
  do sendMsg cs (ircLinks (Text.pack <$> words rest))
     commandSuccess st

cmdTime :: NetworkCommand
cmdTime _ cs st rest =
  do sendMsg cs (ircTime (Text.pack <$> words rest))
     commandSuccess st

cmdZnc :: NetworkCommand
cmdZnc _ cs st rest =
  do sendMsg cs (ircZnc (Text.words (Text.pack rest)))
     commandSuccess st

-- TODO: support time ranges
cmdZncPlayback :: NetworkCommand
cmdZncPlayback _ cs st rest =
  case words rest of

    -- request everything
    [] -> success "0"

    -- current date explicit time
    [timeStr]
       | Just tod <- parse timeFormats timeStr ->
          do now <- getZonedTime
             successZoned
               (set (zonedTimeLocalTime . localTimeTimeOfDay) tod now)

    -- explicit date and time
    [dateStr,timeStr]
       | Just day  <- parse dateFormats dateStr
       , Just tod  <- parse timeFormats timeStr ->
          do tz <- getCurrentTimeZone
             successZoned ZonedTime
               { zonedTimeZone = tz
               , zonedTimeToLocalTime = LocalTime
                   { localTimeOfDay = tod
                   , localDay       = day } }

    _ -> commandFailure st

  where
    -- %k doesn't require a leading 0 for times before 10AM
    timeFormats = ["%k:%M:%S","%k:%M"]
    dateFormats = ["%F"]
    parse formats str =
      asum (map (parseTimeM False defaultTimeLocale ?? str) formats)

    successZoned = success . formatTime defaultTimeLocale "%s"

    success start =
      do sendMsg cs (ircZnc ["*playback", "play", "*", Text.pack start])
         commandSuccess st

cmdMode :: NetworkCommand
cmdMode _ cs st rest = modeCommand (Text.pack <$> words rest) cs st

cmdNick :: NetworkCommand
cmdNick _ cs st rest =
  case words rest of
    [nick] ->
      do sendMsg cs (ircNick (mkId (Text.pack nick)))
         commandSuccess st
    _ -> commandFailure st

cmdPart :: ChannelCommand
cmdPart _ cs channelId st rest =
  do let msg = dropWhile isSpace rest
     sendMsg cs (ircPart channelId (Text.pack msg))
     commandSuccess st

-- | This command is equivalent to chatting without a command. The primary use
-- at the moment is to be able to send a leading @/@ to chat easily.
cmdSay :: ChannelCommand
cmdSay _network _cs _channelId st rest = executeChat rest st

cmdInvite :: ChannelCommand
cmdInvite _ cs channelId st rest =
  case words rest of
    [nick] ->
      do let freeTarget = has (csChannels . ix channelId . chanModes . ix 'g') cs
             cmd = ircInvite (Text.pack nick) channelId
         cs' <- if freeTarget
                  then cs <$ sendMsg cs cmd
                  else sendModeration channelId [cmd] cs
         commandSuccessUpdateCS cs' st

    _ -> commandFailure st

commandSuccessUpdateCS :: NetworkState    -> ClientState -> IO CommandResult
commandSuccessUpdateCS cs st =
  let networkId = view csNetworkId cs in
  commandSuccess
    $ setStrict (clientConnections . ix networkId) cs st

cmdTopic :: ChannelCommand
cmdTopic _ cs channelId st rest =
  do let cmd =
           case dropWhile isSpace rest of
             ""    -> ircTopic channelId ""
             topic | useChanServ channelId cs ->
                        ircPrivmsg (mkId "ChanServ")
                          ("TOPIC " <> idText channelId <> Text.pack (' ' : topic))
                   | otherwise -> ircTopic channelId (Text.pack topic)
     sendMsg cs cmd
     commandSuccess st

tabTopic ::
  Bool {- ^ reversed -} ->
  ChannelCommand
tabTopic _ _ cs channelId st rest

  | all isSpace rest
  , Just topic <- preview (csChannels . ix channelId . chanTopic) cs =
     do let textBox = set Edit.line (Edit.endLine $ "/topic " ++ Text.unpack topic)
        commandSuccess (over clientTextBox textBox st)

  | otherwise = commandFailure st


cmdUsers :: ChannelCommand
cmdUsers _ _ _ st _ = commandSuccess (changeSubfocus FocusUsers st)

cmdChannelInfo :: ChannelCommand
cmdChannelInfo _ _ _ st _ = commandSuccess (changeSubfocus FocusInfo st)

cmdMasks :: ChannelCommand
cmdMasks _ cs _ st rest =
  case words rest of
    [[mode]] | mode `elem` view (csModeTypes . modesLists) cs ->
        commandSuccess (changeSubfocus (FocusMasks mode) st)
    _ -> commandFailure st

cmdKick :: ChannelCommand
cmdKick _ cs channelId st rest =
  case nextWord rest of
    Nothing -> commandFailure st
    Just (who,reason) ->
      do let msg = Text.pack (dropWhile isSpace reason)
             cmd = ircKick channelId (Text.pack who) msg
         cs' <- sendModeration channelId [cmd] cs
         commandSuccessUpdateCS cs' st


cmdKickBan :: ChannelCommand
cmdKickBan _ cs channelId st rest =
  case nextWord rest of
    Nothing -> commandFailure st
    Just (whoStr,reason) ->
      do let msg = Text.pack (dropWhile isSpace reason)

             whoTxt     = Text.pack whoStr

             mask = renderUserInfo (computeBanUserInfo (mkId whoTxt) cs)
             cmds = [ ircMode channelId ["b", mask]
                    , ircKick channelId whoTxt msg
                    ]
         cs' <- sendModeration channelId cmds cs
         commandSuccessUpdateCS cs' st

computeBanUserInfo :: Identifier -> NetworkState    -> UserInfo
computeBanUserInfo who cs =
  case view (csUser who) cs of
    Nothing                   -> UserInfo who        "*" "*"
    Just (UserAndHost _ host) -> UserInfo (mkId "*") "*" host

cmdRemove :: ChannelCommand
cmdRemove _ cs channelId st rest =
  case nextWord rest of
    Nothing -> commandFailure st
    Just (who,reason) ->
      do let msg = Text.pack (dropWhile isSpace reason)
             cmd = ircRemove channelId (Text.pack who) msg
         cs' <- sendModeration channelId [cmd] cs
         commandSuccessUpdateCS cs' st

cmdJoin :: NetworkCommand
cmdJoin network cs st rest =
  let ws = words rest
      doJoin channelStr keyStr =
        do let channelId = mkId (Text.pack (takeWhile (/=',') channelStr))
           sendMsg cs (ircJoin (Text.pack channelStr) (Text.pack <$> keyStr))
           commandSuccess
               $ changeFocus (ChannelFocus network channelId) st
  in case ws of
       [channel]     -> doJoin channel Nothing
       [channel,key] -> doJoin channel (Just key)
       _             -> commandFailure st


-- | @/channel@ command. Takes the name of a channel and switches
-- focus to that channel on the current network.
cmdChannel :: NetworkCommand
cmdChannel network _ st rest =
  case mkId . Text.pack <$> words rest of
    [ channelId ] ->
       commandSuccess
         $ changeFocus (ChannelFocus network channelId) st
    _ -> commandFailure st


cmdQuit :: NetworkCommand
cmdQuit _ cs st rest =
  do let msg = Text.pack (dropWhile isSpace rest)
     sendMsg cs (ircQuit msg)
     commandSuccess st

cmdDisconnect :: NetworkCommand
cmdDisconnect network _ st _ =
  do st' <- abortNetwork network st
     commandSuccess st'

-- | Reconnect to the currently focused network. It's possible
-- that we're not currently connected to a network, so
-- this is implemented as a client command.
cmdReconnect :: ClientCommand
cmdReconnect st _
  | Just network <- views clientFocus focusNetwork st =

      do st' <- addConnection network =<< abortNetwork network st
         commandSuccess
           $ changeFocus (NetworkFocus network) st'

  | otherwise = commandFailure st

cmdIgnore :: ClientCommand
cmdIgnore st rest =
  case mkId . Text.pack <$> words rest of
    [] -> commandFailure st
    xs -> commandSuccess
            $ over clientIgnores updateIgnores st
      where
        updateIgnores :: HashSet Identifier -> HashSet Identifier
        updateIgnores s = foldl' updateIgnore s xs
        updateIgnore s x = over (contains x) not s

-- | Implementation of @/reload@
--
-- Attempt to reload the configuration file
cmdReload :: ClientCommand
cmdReload st rest =
  do let path | null rest = view (clientConfig . configConfigPath) st
              | otherwise = Just rest
     res <- loadConfiguration path
     case res of
       Left{} -> commandFailure st
       Right cfg ->
         do st1 <- clientStartExtensions (set clientConfig cfg st)
            commandSuccess st1

-- | Support file name tab completion when providing an alternative
-- configuration file.
--
-- /NOT IMPLEMENTED/
tabReload :: Bool {- ^ reversed -} -> ClientCommand
tabReload _ st _ = commandFailure st

modeCommand ::
  [Text] {- mode parameters -} ->
  NetworkState                 ->
  ClientState                  ->
  IO CommandResult
modeCommand modes cs st =
  case view clientFocus st of

    NetworkFocus _ ->
      do sendMsg cs (ircMode (view csNick cs) modes)
         commandSuccess st

    ChannelFocus _ chan ->
      case modes of
        [] -> success False [[]]
        flags:params ->
          case splitModes (view csModeTypes cs) flags params of
            Nothing -> commandFailure st
            Just parsedModes ->
              success needOp (unsplitModes <$> chunksOf (view csModeCount cs) parsedModes')
              where
                parsedModes'
                  | useChanServ chan cs = filter (not . isOpMe) parsedModes
                  | otherwise           = parsedModes

                needOp = not (all isPublicChannelMode parsedModes)
      where
        isOpMe (True, 'o', param) = mkId param == view csNick cs
        isOpMe _                  = False

        success needOp argss =
          do let cmds = ircMode chan <$> argss
             cs' <- if needOp
                      then sendModeration chan cmds cs
                      else cs <$ traverse_ (sendMsg cs) cmds
             commandSuccessUpdateCS cs' st

    _ -> commandFailure st

tabMode :: Bool -> NetworkCommand
tabMode isReversed _ cs st rest =
  case view clientFocus st of

    ChannelFocus _ channel
      | flags:params     <- Text.words (Text.pack rest)
      , Just parsedModes <- splitModes (view csModeTypes cs) flags params
      , let parsedModesWithParams =
              [ (pol,mode) | (pol,mode,arg) <- parsedModes, not (Text.null arg) ]
      , (pol,mode):_      <- drop (paramIndex-3) parsedModesWithParams
      , let (hint, completions) = computeModeCompletion pol mode channel cs st
      -> commandSuccess
       $ fromMaybe st
       $ clientTextBox (wordComplete id isReversed hint completions) st

    _ -> commandSuccess st

  where
    paramIndex = length $ words $ uncurry take $ clientLine st

activeNicks ::
  ClientState ->
  [Identifier]
activeNicks st =
  case view clientFocus st of
    focus@(ChannelFocus network channel) ->
      toListOf
        ( clientWindows    . ix focus
        . winMessages      . folded
        . wlBody           . _IrcBody
        . folding msgActor . to userNick
        . filtered isActive) st
      where
        isActive n = HashMap.member n userMap
        userMap = view ( clientConnection network
                       . csChannels . ix channel
                       . chanUsers) st

    _ -> []

-- | Use the *!*@host masks of users for channel lists when setting list modes
--
-- Use the channel's mask list for removing modes
--
-- Use the nick list otherwise
computeModeCompletion ::
  Bool {- ^ mode polarity -} ->
  Char {- ^ mode          -} ->
  Identifier {- ^ channel -} ->
  NetworkState    ->
  ClientState ->
  ([Identifier],[Identifier]) {- ^ (hint, complete) -}
computeModeCompletion pol mode channel cs st
  | mode `elem` view modesLists modeSettings =
        if pol then ([],usermasks) else ([],masks)
  | otherwise = (activeNicks st, nicks)
  where
    modeSettings = view csModeTypes cs
    nicks = HashMap.keys (view (csChannels . ix channel . chanUsers) cs)

    masks = mkId <$> HashMap.keys (view (csChannels . ix channel . chanLists . ix mode) cs)

    usermasks =
      [ mkId ("*!*@" <> host)
        | nick <- HashMap.keys (view (csChannels . ix channel . chanUsers) cs)
        , UserAndHost _ host <- toListOf (csUsers . ix nick) cs
        ]

-- | Predicate for mode commands that can be performed without ops
isPublicChannelMode :: (Bool, Char, Text) -> Bool
isPublicChannelMode (True, 'b', param) = Text.null param -- query ban list
isPublicChannelMode (True, 'q', param) = Text.null param -- query quiet list
isPublicChannelMode _                  = False

commandNameCompletion :: Bool -> ClientState -> Maybe ClientState
commandNameCompletion isReversed st =
  do guard (cursorPos == n)
     clientTextBox (wordComplete id isReversed [] possibilities) st
  where
    n = length leadingPart
    (cursorPos, line) = clientLine st
    leadingPart = takeWhile (not . isSpace) line
    possibilities = mkId . Text.cons '/' <$> commandNames
    commandNames = HashMap.keys commands
                ++ HashMap.keys (view (clientConfig . configMacros) st)

-- | Complete the nickname at the current cursor position using the
-- userlist for the currently focused channel (if any)
nickTabCompletion :: Bool {- ^ reversed -} -> ClientState -> ClientState
nickTabCompletion isReversed st
  = fromMaybe st
  $ clientTextBox (wordComplete (++": ") isReversed hint completions) st
  where
    hint        = activeNicks st
    completions = currentCompletionList st

-- | Used to send commands that require ops to perform.
-- If this channel is one that the user has chanserv access and ops are needed
-- then ops are requested and the commands are queued, otherwise send them
-- directly.
sendModeration ::
  Identifier      {- ^ channel       -} ->
  [RawIrcMsg]     {- ^ commands      -} ->
  NetworkState    {- ^ network state -} ->
  IO NetworkState
sendModeration channel cmds cs
  | useChanServ channel cs =
      do sendMsg cs (ircPrivmsg (mkId "ChanServ") ("OP " <> idText channel))
         return $ csChannels . ix channel . chanQueuedModeration <>~ cmds $ cs
  | otherwise = cs <$ traverse_ (sendMsg cs) cmds

useChanServ :: Identifier -> NetworkState    -> Bool
useChanServ channel cs =
  channel `elem` view (csSettings . ssChanservChannels) cs &&
  not (iHaveOp channel cs)

cmdExtension :: ClientCommand
cmdExtension st rest =
  case Text.words (Text.pack rest) of
    name:params
      | Just ae <- find (\ae -> aeName ae == name) (view clientExtensions st) ->
         do (st',_) <- withStableMVar st $ \stab ->
                         commandExtension stab params ae
            commandSuccess st'
    _ -> commandFailure st

