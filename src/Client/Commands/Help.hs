{-# LANGUAGE TransformListComp, OverloadedStrings #-}

{-|
Module      : Client.Commands.Help
Description : Implementation of the /help command
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

Implements the /help command, which is responsible for managing the help buffer.
Unfortunately, this makes this command somewhat unique in that it's responsible for rendering.
-}

module Client.Commands.Help ( cmdHelp ) where

import           Client.Commands.Arguments.Renderer
import           Client.Commands.Arguments.Spec
import           Client.Commands.Interpolation
import           Client.Commands.Recognizer
import           Client.Commands.Types
import           Client.Configuration (configMacros)
import           Client.Image.MircFormatting
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.State
import           Client.State.Focus (focusNetwork, Subfocus(FocusHelp))
import           Client.State.Help
import           Client.State.Network (sendMsg)
import           Control.Lens
import           Data.Foldable (toList)
import           Data.List (delete, intercalate, sortOn)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Graphics.Vty.Attributes
import           Irc.RawIrcMsg (rawIrcMsg)

displayHelp :: ClientState -> HelpState -> IO CommandResult
displayHelp st help = commandSuccess . changeSubfocus FocusHelp $ set clientHelp help st

-- | Implementation of @/help@ command.
cmdHelp :: [CommandSection] -> Recognizer Command -> WindowCommand (Maybe String)
cmdHelp commandsList commands focus st (Just (':':queryStr)) = case focusNetwork focus of
  Nothing -> commandFailureMsg "empty network prefix requires focused network" st
  Just net -> cmdHelp commandsList commands focus st (Just $ Text.unpack net ++ (':':queryStr)) -- Network name better not start with a colon! ;)
cmdHelp _ commands _ st (Just queryStr)
  | Just queryText == savedQueryText = commandSuccess (changeSubfocus FocusHelp st)
  | otherwise = loadHelp commands queryText st
  where
    savedQueryText = helpQueryToText $ view (clientHelp . hsQuery) st
    queryText = Text.pack queryStr
cmdHelp commandsList _ _ st Nothing = loadHelpList commandsList st

loadHelpList :: [CommandSection] -> ClientState -> IO CommandResult
loadHelpList commandList st = displayHelp st $ makeHelp Nothing $ listAllCommands commandList st

loadHelp :: Recognizer Command -> Text -> ClientState -> IO CommandResult
loadHelp commands query st = case Text.break (==':') query of
  (cmdName,"") -> loadHelpCmd commands cmdName st
  (net,topic) -> sendHelpQuery net (Text.tail topic) st

sendHelpQuery :: Text -> Text -> ClientState -> IO CommandResult
sendHelpQuery net topic st = case preview (clientConnection net) st of
  Just cs -> do
    sendMsg cs (rawIrcMsg "HELP" [topic]) -- TODO: Turns out irc-core doesn't have an ircHelp function.
    displayHelp st $ awaitHelp net topic
  Nothing -> commandFailureMsg (Text.append "not connected to " net) st

loadHelpCmd :: Recognizer Command -> Text -> ClientState -> IO CommandResult
loadHelpCmd commands cmdName st = case recognize cmdName commands of
  Invalid -> commandFailureMsg "unknown command, try /help with no argument" st
  Prefix sfxs -> commandFailureMsg (Text.append "unknown command, did you mean: " suggestions) st
    where suggestions = Text.intercalate " " ((cmdName <>) <$> sfxs)
  Exact cmd -> displayHelp st $ makeHelp (Just cmdName) $ commandHelpLines st cmdName cmd

-- | Generate detailed help lines for the command with the given name.
commandHelpLines ::
  ClientState {- ^ client state -} ->
  Text        {- ^ command name -} ->
  Command     {- ^ command      -} ->
  [Image']    {- ^ lines        -}
commandHelpLines
  st
  cmdName
  Command{cmdNames = names, cmdImplementation = impl, cmdArgumentSpec = spec, cmdDocumentation = doc}
  = reverse $ heading "Syntax: " <> commandSummary (makeArgsContext st) pal (pure cmdName) spec
      : emptyLine
      : aliasLines
     ++ explainContext impl
      : emptyLine
      : map (parseIrcText pal) (Text.lines doc)
  where
    pal = clientPalette st
    aliasLines =
      case delete cmdName (toList names) of
        [] -> []
        ns -> [ heading "Aliases: " <> text' defAttr (Text.intercalate ", " ns), emptyLine ]

heading :: Text -> Image'
heading = text' (withStyle defAttr bold)

-- | Generate an explanation of the context where the given command
-- implementation will be valid.
explainContext ::
  CommandImpl a {- ^ command implementation -} ->
  Image'        {- ^ help line              -}
explainContext impl =
  heading "Context: " <>
  case impl of
    ClientCommand {}   -> "client (works everywhere)"
    WindowCommand {}   -> "window (works on the current window)"
    NetworkCommand{}   -> "network (works when focused on active network)"
    MaybeChatCommand{} -> "network (works when focused on active network)" -- Intentional duplicate.
    ChatCommand{}      -> "chat (works when focused on an active channel or private message)"
    ChannelCommand{}   -> "channel (works when focused on active channel)"

-- | Generate the lines for the help window showing all commands.
listAllCommands ::
  [CommandSection] {- ^  commands  -} ->
  ClientState {- ^ client state    -} ->
  [Image']    {- ^ help lines      -}
listAllCommands commandsList st
  = intercalate [emptyLine]
  $ map reverse
  $ (listCommandSection st pal <$> commandsList)
 ++ [macroCommandSection st pal]
 where pal = clientPalette st

macroCommandSection ::
  ClientState    {- ^ client state    -} ->
  Palette        {- ^ palette         -} ->
  [Image']       {- ^ help lines      -}
macroCommandSection st pal
  | null macros = []
  | otherwise =
      text' (withStyle defAttr bold) "Macros" :
      [ commandSummary st pal (pure name) spec
      | Macro name (MacroSpec spec) _ <- macros
      , then sortOn by name
      ]
  where
    macros = toListOf (clientConfig . configMacros . folded) st

listCommandSection ::
  ClientState    {- ^ client state    -} ->
  Palette        {- ^ palette         -} ->
  CommandSection {- ^ command section -} ->
  [Image']       {- ^ help lines      -}
listCommandSection st pal sec
  = text' (withStyle defAttr bold) (cmdSectionName sec)
  : [ commandSummary (makeArgsContext st) pal names spec
    | -- pattern needed due to existential quantification
      Command { cmdNames        = names
              , cmdArgumentSpec = spec
              } <- cmdSectionCmds sec
    ]

-- | Generate the help line for the given command and its
-- specification for use in the list of commands.
commandSummary ::
  r                {- ^ client state             -} ->
  Palette          {- ^ palette                  -} ->
  NonEmpty Text    {- ^ command name and aliases -} ->
  Args r a         {- ^ argument specification   -} ->
  Image'           {- ^ summary help line        -}
commandSummary st pal (cmd :| _) args  =
  char defAttr '/' <>
  text' (view palCommandReady pal) cmd <>
  render pal' st True args ""

  where
    pal' = set palCommandPlaceholder defAttr pal

-- Empty line used as a separator
emptyLine :: Image'
emptyLine = char defAttr ' '
