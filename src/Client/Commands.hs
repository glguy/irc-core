{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell, ExistentialQuantification #-}

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
  , commandExpansion
  , tabCompletion
  -- * Commands
  , CommandSection(..)
  , Command(..)
  , CommandImpl(..)
  , commands
  , commandsList
  ) where

import Client.Commands.Arguments.Parser (parse)
import Client.Commands.Arguments.Spec (optionalArg, optionalNumberArg, remainingArg, simpleToken, extensionArg, mapArgEnv, Args)
import Client.Commands.Docs (clientDocs, cmdDoc)
import Client.Commands.Exec
import Client.Commands.Interpolation (resolveMacroExpansions, Macro(Macro), MacroSpec(MacroSpec))
import Client.Commands.Recognizer (fromCommands, keys, recognize, Recognition(Exact), Recognizer)
import Client.Commands.WordCompletion (caseText, plainWordCompleteMode, wordComplete)
import Client.Configuration
import Client.State
import Client.State.Extensions (clientCommandExtension, clientStartExtensions)
import Client.State.Focus
import Client.State.Network (csNick, isChannelIdentifier, sendMsg)
import Client.State.Url
import Control.Applicative (liftA2, (<|>))
import Control.Exception (displayException, try)
import Control.Lens
import Control.Monad (guard, foldM)
import Data.Foldable (foldl', toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (getZonedTime)
import Irc.Commands (ircPrivmsg)
import Irc.Identifier (idText)
import Irc.Message (IrcMsg(Privmsg))
import Irc.RawIrcMsg (parseRawIrcMsg)
import RtsStats (getStats)
import System.Process.Typed (proc, runProcess_)

import Client.Commands.Certificate (newCertificateCommand)
import Client.Commands.Channel (channelCommands)
import Client.Commands.Chat (chatCommands, chatCommand', executeChat)
import Client.Commands.Connection (connectionCommands)
import Client.Commands.Operator (operatorCommands)
import Client.Commands.Queries (queryCommands)
import Client.Commands.TabCompletion
import Client.Commands.Toggles (togglesCommands)
import Client.Commands.Types
import Client.Commands.Window (windowCommands, parseFocus, focusNames)
import Client.Commands.ZNC (zncCommands)

-- | Interpret the given chat message or command. Leading @/@ indicates a
-- command. Otherwise if a channel or user query is focused a chat message will
-- be sent. Leading spaces before the @/@ are ignored when checking for
-- commands.
execute ::
  String           {- ^ chat or command -} ->
  ClientState      {- ^ client state    -} ->
  IO CommandResult {- ^ command result  -}
execute str st =
  let st' = set clientErrorMsg Nothing st in
  case dropWhile (' '==) str of
    []          -> commandFailure st
    '/':command -> executeUserCommand Nothing command st'
    _           -> executeChat (view clientFocus st') str st'

-- | Execute command provided by user, resolve aliases if necessary.
--
-- The last disconnection time is stored in text form and is available
-- for substitutions in macros. It is only provided when running startup
-- commands during a reconnect event.
executeUserCommand ::
  Maybe Text       {- ^ disconnection time -} ->
  String           {- ^ command            -} ->
  ClientState      {- ^ client state       -} ->
  IO CommandResult {- ^ command result     -}
executeUserCommand = executeUserCommandIn Nothing

-- | Execute command provided by user, resolve aliases if necessary,
-- optionally in the provided focus instead of the current one.
--
-- The last disconnection time is stored in text form and is available
-- for substitutions in macros. It is only provided when running startup
-- commands during a reconnect event.
executeUserCommandIn ::
  Maybe Focus      {- ^ focus override     -} ->
  Maybe Text       {- ^ disconnection time -} ->
  String           {- ^ command            -} ->
  ClientState      {- ^ client state       -} ->
  IO CommandResult {- ^ command result     -}
executeUserCommandIn focusOverride discoTime command st = do
  let key = Text.takeWhile (/=' ') (Text.pack command)
      rest = dropWhile (==' ') (dropWhile (/=' ') command)

  case views (clientConfig . configMacros) (recognize key) st of
    Exact (Macro _ (MacroSpec spec) cmdExs) ->
      case doExpansion spec cmdExs rest of
        Nothing   -> commandFailureMsg "macro expansions failed" st
        Just cmds -> process cmds st
    _ -> executeCommand Nothing focusOverride command st
  where
    doExpansion spec cmdExs rest =
      do args <- parse st spec rest
         traverse (resolveMacro (map Text.pack args)) cmdExs

    resolveMacro args = resolveMacroExpansions (commandExpansion discoTime st) (expandInt args)

    expandInt :: [a] -> Integer -> Maybe a
    expandInt args i = preview (ix (fromInteger i)) args

    process [] st0 = commandSuccess st0
    process (c:cs) st0 =
      do res <- executeCommand Nothing focusOverride (Text.unpack c) st0
         case res of
           CommandSuccess st1 -> process cs st1
           CommandFailure st1 -> process cs st1 -- ?
           CommandQuit st1    -> return (CommandQuit st1)

-- | Compute the replacement value for the given expansion variable.
commandExpansion ::
  Maybe Text  {- ^ disconnect time    -} ->
  ClientState {- ^ client state       -} ->
  Text        {- ^ expansion variable -} ->
  Maybe Text  {- ^ expansion value    -}
commandExpansion discoTime st v =
  case v of
    "network" -> views clientFocus focusNetwork st
    "channel" -> previews (clientFocus . _ChannelFocus . _2) idText st
    "nick"    -> do net <- views clientFocus focusNetwork st
                    cs  <- preview (clientConnection net) st
                    return (views csNick idText cs)
    "disconnect" -> discoTime
    _         -> Nothing


-- | Respond to the TAB key being pressed. This can dispatch to a command
-- specific completion mode when relevant. Otherwise this will complete
-- input based on the users of the channel related to the current buffer.
tabCompletion ::
  Bool             {- ^ reversed       -} ->
  ClientState      {- ^ client state   -} ->
  IO CommandResult {- ^ command result -}
tabCompletion isReversed st =
  case dropWhile (' ' ==) $ snd $ clientLine st of
    '/':command -> executeCommand (Just isReversed) Nothing command st
    _           -> nickTabCompletion isReversed st

data ContextFreeCommand = forall a. ContextFreeCommand
  { cfCmdCtx  :: ArgsContext
  , cfCmdArgs :: Args ArgsContext a
  , cfCmdIsClientCmd :: Bool 
  , cfCmdExec :: ClientState -> a -> IO CommandResult
  , cfCmdTab  :: Bool -> ClientState -> String -> IO CommandResult
  }

executeContextFreeCommand :: ContextFreeCommand -> Maybe Bool -> String -> IO CommandResult
executeContextFreeCommand ContextFreeCommand{cfCmdCtx=ctx, cfCmdArgs=spec, cfCmdExec=exec, cfCmdTab=tab} tabComplete args =
  case tabComplete of
    Just isReversed -> tab isReversed (argsContextSt ctx) args
    Nothing ->
      case parse ctx spec args of
        Nothing -> commandFailureMsg "bad command arguments" (argsContextSt ctx)
        Just arg -> exec (argsContextSt ctx) arg

cfCmdAsArgs :: ContextFreeCommand -> Args ArgsContext (ClientState -> IO CommandResult)
cfCmdAsArgs ContextFreeCommand{cfCmdArgs=spec, cfCmdExec=exec} = fmap (flip exec) spec

-- | Look up a command by name and return a @ContextFreeCommand@.
prepareCommand :: Focus -> String -> ClientState -> Either Text ContextFreeCommand
prepareCommand focus cmd st =
  case recognize (Text.toLower $ Text.pack cmd) commands of
    Exact Command{cmdImplementation=impl, cmdArgumentSpec=argSpec} ->
      let
        cfCmd exec tab isClient = Right $ ContextFreeCommand
          { cfCmdCtx=ArgsContext {argsContextSt=st, argsContextFocus=focus}
          , cfCmdArgs=argSpec
          , cfCmdExec=exec
          , cfCmdTab=tab
          , cfCmdIsClientCmd=isClient
          }
      in case impl of
        ClientCommand exec tab ->
          cfCmd exec tab True

        WindowCommand exec tab ->
          cfCmd (exec focus) (`tab` focus) False

        NetworkCommand exec tab
          | Just network <- focusNetwork focus
          , Just cs      <- preview (clientConnection network) st ->
              cfCmd (exec cs) (`tab` cs) False
          | otherwise -> Left "command requires focused network"

        MaybeChatCommand exec tab
          | Just cs <- maybeNetwork ->
              cfCmd (exec maybeChat cs) (\x -> tab x maybeChat cs) False
          | otherwise -> Left "command requires focused network"
          where
            maybeChat
              | ChannelFocus _ channel <- focus = Just channel
              | otherwise = Nothing
            maybeNetwork = do
              network <- focusNetwork focus
              preview (clientConnection network) st

        ChannelCommand exec tab
          | ChannelFocus network channelId <- focus
          , Just cs <- preview (clientConnection network) st
          , isChannelIdentifier cs channelId ->
              cfCmd (exec channelId cs) (\x -> tab x channelId cs) False
          | otherwise -> Left "command requires focused channel"

        ChatCommand exec tab
          | ChannelFocus network channelId <- focus
          , Just cs <- preview (clientConnection network) st ->
              cfCmd (exec channelId cs) (\x -> tab x channelId cs) False
          | otherwise -> Left "command requires focused chat window"

    _ -> Left "unknown command"

-- | Parse and execute the given command. When the first argument is Nothing
-- the command is executed, otherwise the first argument is the cursor
-- position for tab-completion
executeCommand ::
  Maybe Bool       {- ^ tab-completion direction -} ->
  Maybe Focus      {- ^ focus override           -} ->
  String           {- ^ command                  -} ->
  ClientState      {- ^ client state             -} ->
  IO CommandResult {- ^ command result           -}

executeCommand (Just isReversed) _ _ st
  | Just st' <- commandNameCompletion isReversed st = commandSuccess st'

executeCommand tabComplete focusOverride str st =
  let (cmd, args) = break (==' ') str
      focus       = fromMaybe (view clientFocus st) focusOverride
  in case prepareCommand focus cmd st of
    Right cfCmd -> executeContextFreeCommand cfCmd tabComplete args
    Left errmsg -> case tabComplete of
      Just isReversed -> nickTabCompletion isReversed st
      Nothing         -> commandFailureMsg errmsg st

-- | Expands each alias to have its own copy of the command callbacks
expandAliases :: [Command] -> [(Text,Command)]
expandAliases xs =
  [ (name, cmd) | cmd <- xs, name <- toList (cmdNames cmd) ]


-- | Map of built-in client commands to their implementations, tab completion
-- logic, and argument structures.
commands :: Recognizer Command
commands = fromCommands (expandAliases (concatMap cmdSectionCmds commandsList))


-- | Raw list of commands in the order used for @/help@
commandsList :: [CommandSection]
commandsList =

  ------------------------------------------------------------------------
  [ CommandSection "Client commands"
  ------------------------------------------------------------------------

  [ Command
      (pure "exit")
      (pure ())
      $(clientDocs `cmdDoc` "exit")
    $ ClientCommand cmdExit noClientTab

  , Command
      (pure "reload")
      (optionalArg (simpleToken "[filename]"))
      $(clientDocs `cmdDoc` "reload")
    $ ClientCommand cmdReload tabReload

  , Command
      (pure "in")
      (extensionArg "focus command" inArgs)
      $(clientDocs `cmdDoc` "in")
    $ ClientCommand cmdIn tabIn

  , Command
      (pure "extension")
      (liftA2 (,) (simpleToken "extension") (remainingArg "arguments"))
      $(clientDocs `cmdDoc` "extension")
    $ ClientCommand cmdExtension simpleClientTab

  , Command
      (pure "palette")
      (pure ())
      $(clientDocs `cmdDoc` "palette")
    $ ClientCommand cmdPalette noClientTab

  , Command
      (pure "digraphs")
      (pure ())
      $(clientDocs `cmdDoc` "digraphs")
    $ ClientCommand cmdDigraphs noClientTab

  , Command
      (pure "keymap")
      (pure ())
      $(clientDocs `cmdDoc` "keymap")
    $ ClientCommand cmdKeyMap noClientTab

  , Command
      (pure "rtsstats")
      (pure ())
      $(clientDocs `cmdDoc` "rtsstats")
    $ ClientCommand cmdRtsStats noClientTab

  , Command
      (pure "exec")
      (remainingArg "arguments")
      $(clientDocs `cmdDoc` "exec")
    $ ClientCommand cmdExec simpleClientTab

  , Command
      (pure "url")
      optionalNumberArg
      $(clientDocs `cmdDoc` "url")
    $ ClientCommand cmdUrl noClientTab

  , newCertificateCommand

  , Command
      (pure "help")
      (optionalArg (simpleToken "[command]"))
      $(clientDocs `cmdDoc` "help")
    $ ClientCommand cmdHelp tabHelp

  ------------------------------------------------------------------------
  ],

  togglesCommands, connectionCommands, windowCommands, chatCommands,
  queryCommands, channelCommands, zncCommands, operatorCommands
  ]

-- | Implementation of @/exit@ command.
cmdExit :: ClientCommand ()
cmdExit st _ = return (CommandQuit st)

-- | Implementation of @/palette@ command. Set subfocus to Palette.
cmdPalette :: ClientCommand ()
cmdPalette st _ = commandSuccess (changeSubfocus FocusPalette st)

-- | Implementation of @/digraphs@ command. Set subfocus to Digraphs.
cmdDigraphs :: ClientCommand ()
cmdDigraphs st _ = commandSuccess (changeSubfocus FocusDigraphs st)

-- | Implementation of @/keymap@ command. Set subfocus to Keymap.
cmdKeyMap :: ClientCommand ()
cmdKeyMap st _ = commandSuccess (changeSubfocus FocusKeyMap st)

-- | Implementation of @/rtsstats@ command. Set subfocus to RtsStats.
-- Update cached rts stats in client state.
cmdRtsStats :: ClientCommand ()
cmdRtsStats st _ =
  do mb <- getStats
     case mb of
       Nothing -> commandFailureMsg "RTS statistics not available. (Use +RTS -T)" st
       Just{}  -> commandSuccess $ set clientRtsStats mb
                                 $ changeSubfocus FocusRtsStats st

-- | Implementation of @/help@ command. Set subfocus to Help.
cmdHelp :: ClientCommand (Maybe String)
cmdHelp st mb = commandSuccess (changeSubfocus focus st)
  where
    focus = FocusHelp (fmap Text.pack mb)

tabHelp :: Bool -> ClientCommand String
tabHelp isReversed st _ =
  simpleTabCompletion plainWordCompleteMode [] commandNames isReversed st
  where
    commandNames = fst <$> expandAliases (concatMap cmdSectionCmds commandsList)

-- | Implementation of @/reload@
--
-- Attempt to reload the configuration file
cmdReload :: ClientCommand (Maybe String)
cmdReload st mbPath =
  do let path = mbPath <|> Just (view clientConfigPath st)
     res <- loadConfiguration path
     case res of
       Left e -> commandFailureMsg (describeProblem e) st
       Right (path',cfg) ->
         do st1 <- clientStartExtensions
                 $ set clientConfig cfg
                 $ set clientConfigPath path' st
            commandSuccess st1

  where
    describeProblem err =
      Text.pack $
      case err of
       ConfigurationReadFailed    e -> "Failed to open configuration: "  ++ e
       ConfigurationParseFailed _ e -> "Failed to parse configuration: " ++ e
       ConfigurationMalformed   _ e -> "Configuration malformed: "       ++ e

-- | Support file name tab completion when providing an alternative
-- configuration file.
--
-- /NOT IMPLEMENTED/
tabReload :: Bool {- ^ reversed -} -> ClientCommand String
tabReload _ st _ = commandFailure st

commandNameCompletion :: Bool -> ClientState -> Maybe ClientState
commandNameCompletion isReversed st =
  do guard (cursorPos == n)
     clientTextBox (wordComplete (' ' /=) plainWordCompleteMode isReversed [] possibilities) st
  where
    n = length white + length leadingPart
    (cursorPos, line) = clientLine st
    (white, leadingPart) = takeWhile (' ' /=) <$> span (' '==) line

    possibilities = caseText . Text.cons '/' <$> commandNames
    commandNames = keys commands
                ++ keys (view (clientConfig . configMacros) st)

cmdExtension :: ClientCommand (String, String)
cmdExtension st (name,command) =
  do res <- clientCommandExtension (Text.pack name) (Text.pack command) st
     case res of
       Nothing  -> commandFailureMsg "unknown extension" st
       Just st' -> commandSuccess st'

-- | Implementation of @/exec@ command.
cmdExec :: ClientCommand String
cmdExec st rest =
  do now <- getZonedTime
     case parseExecCmd rest of
       Left es -> failure now es
       Right ec ->
         case buildTransmitter now ec of
           Left es -> failure now es
           Right tx ->
             do res <- runExecCmd ec
                case res of
                  Left es -> failure now es
                  Right msgs -> tx (map Text.pack msgs)

  where
    buildTransmitter now ec =
      case (Text.pack <$> view execOutputNetwork ec,
            Text.pack <$> view execOutputChannel ec) of

        (Unspecified, Unspecified) -> Right (sendToClient now)

        (Specified network, Specified channel) ->
          case preview (clientConnection network) st of
            Nothing -> Left ["Unknown network"]
            Just cs -> Right (sendToChannel cs channel)

        (_ , Specified channel) ->
          case currentNetworkState of
            Nothing -> Left ["No current network"]
            Just cs -> Right (sendToChannel cs channel)

        (Specified network, _) ->
          case preview (clientConnection network) st of
            Nothing -> Left ["Unknown network"]
            Just cs -> Right (sendToNetwork now cs)

        (_, Current) ->
          case currentNetworkState of
            Nothing -> Left ["No current network"]
            Just cs ->
              case view clientFocus st of
                ChannelFocus _ channel -> Right (sendToChannel cs (idText channel))
                _                      -> Left ["No current channel"]

        (Current, _) ->
          case currentNetworkState of
            Nothing -> Left ["No current network"]
            Just cs -> Right (sendToNetwork now cs)

    sendToClient now msgs = commandSuccess $! foldl' (recordSuccess now) st msgs

    sendToNetwork now cs msgs =
      commandSuccess =<<
      foldM (\st1 msg ->
           case parseRawIrcMsg msg of
             Nothing ->
               return $! recordError now "" ("Bad raw message: " <> msg) st1
             Just raw ->
               do sendMsg cs raw
                  return st1) st msgs

    sendToChannel cs channel msgs =
      commandSuccess =<<
      foldM (\st1 msg ->
        do sendMsg cs (ircPrivmsg channel msg)
           chatCommand'
              (\src tgt -> Privmsg src tgt msg)
              [channel]
              cs st1) st (filter (not . Text.null) msgs)

    currentNetworkState =
      do network <- views clientFocus focusNetwork st
         preview (clientConnection network) st

    failure now es =
      commandFailure $! foldl' (flip (recordError now "")) st (map Text.pack es)


cmdUrl :: ClientCommand (Maybe Int)
cmdUrl st arg =
  case view (clientConfig . configUrlOpener) st of
    Nothing     -> commandFailureMsg "url-opener not configured" st
    Just opener -> doUrlOpen opener (maybe 0 (subtract 1) arg)
  where
    doUrlOpen opener n =
      case preview (ix n) (map fst (urlList st)) of
        Just url -> openUrl opener (Text.unpack url) st
        Nothing  -> commandFailureMsg "bad url number" st

openUrl :: UrlOpener -> String -> ClientState -> IO CommandResult
openUrl (UrlOpener opener args) url st =
  do let argStr (UrlArgLiteral str) = str
         argStr UrlArgUrl           = url
     res <- try (runProcess_ (proc opener (map argStr args)))
     case res of
       Left e  -> commandFailureMsg (Text.pack (displayException (e :: IOError))) st
       Right{} -> commandSuccess st

inArgs :: ArgsContext -> String -> Maybe (Args ArgsContext (ClientState -> IO CommandResult))
inArgs ArgsContext{argsContextFocus=focus} focusOverride =
  fmap (\f -> mapArgEnv (changeArgsFocus f) $ extensionArg "command" inArgsCmd) parsedFocus
  where
    parsedFocus = parseFocus (focusNetwork focus) focusOverride
    changeArgsFocus focus' argsContext = argsContext {argsContextFocus=focus'}
    filterCfCmd (Right v@ContextFreeCommand{cfCmdIsClientCmd=False}) = Just v
    filterCfCmd _ = Nothing
    inArgsCmd :: ArgsContext -> String -> Maybe (Args ArgsContext (ClientState -> IO CommandResult))
    inArgsCmd ArgsContext{argsContextFocus=focus', argsContextSt=st'} cmdName =
      cfCmdAsArgs <$> (filterCfCmd $ prepareCommand focus' cmdName st')

-- | Implementation of @/in@.
cmdIn :: ClientCommand (ClientState -> IO CommandResult)
cmdIn st fn = fn st -- All the magic is in the Args parser.

tabIn :: Bool -> ClientCommand String
tabIn isReversed st _ = -- TOOD: Command completion. This right here is just tabChannel.
  simpleTabCompletion plainWordCompleteMode [] (focusNames st) isReversed st
