{-# Language BangPatterns, OverloadedStrings, TransformListComp #-}

{-|
Module      : Client.View.Help
Description : Renderer for help lines
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the rendering used for the @/help@ command.

-}
module Client.View.Help
  ( helpImageLines
  ) where

import           Client.State (ClientState, clientConfig)
import           Client.Configuration (configMacros)
import           Client.Commands
import           Client.Commands.Interpolation
import           Client.Commands.Arguments.Spec
import           Client.Commands.Arguments.Renderer
import           Client.Commands.Recognizer
import           Client.Image.MircFormatting
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Control.Lens
import           Data.Foldable (toList)
import           Data.List (delete, intercalate, sortOn)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Graphics.Vty.Attributes

-- | Generate either the list of all commands and their arguments,
-- or when given a command name generate the detailed help text
-- for that command.
helpImageLines ::
  ClientState {- ^ client state          -} ->
  Maybe Text  {- ^ optional command name -} ->
  Palette     {- ^ palette               -} ->
  [Image']    {- ^ help lines            -}
helpImageLines st mbCmd pal =
  case mbCmd of
    Nothing  -> listAllCommands st pal
    Just cmd -> commandHelpLines st cmd pal

-- | Generate detailed help lines for the command with the given name.
commandHelpLines ::
  ClientState {- ^ client state -} ->
  Text        {- ^ command name -} ->
  Palette     {- ^ palette      -} ->
  [Image']    {- ^ lines        -}
commandHelpLines st cmdName pal =
  case recognize cmdName commands of
    Invalid -> [string (view palError pal) "Unknown command, try /help"]
    Prefix sfxs ->
      [string (view palError pal) $ "Unknown command, did you mean: " ++ suggestions]
      where
      suggestions = Text.unpack $ Text.intercalate " " ((cmdName <>) <$> sfxs)
    Exact Command{cmdNames = names, cmdImplementation = impl,
                  cmdArgumentSpec = spec, cmdDocumentation = doc} ->
      reverse $ heading "Syntax: " <> commandSummary st pal (pure cmdName) spec
              : emptyLine
              : aliasLines
             ++ explainContext impl
              : emptyLine
              : map parseIrcText (Text.lines doc)
      where
        aliasLines =
          case delete cmdName (toList names) of
            [] -> []
            ns -> [ heading "Aliases: " <>
                    text' defAttr (Text.intercalate ", " ns)
                  , emptyLine ]

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
    ClientCommand {} -> "client (works everywhere)"
    NetworkCommand{} -> "network (works when focused on active network)"
    ChannelCommand{} -> "channel (works when focused on active channel)"
    ChatCommand   {} -> "chat (works when focused on an active channel or private message)"


-- | Generate the lines for the help window showing all commands.
listAllCommands ::
  ClientState {- ^ client state    -} ->
  Palette     {- ^ palette         -} ->
  [Image']    {- ^ help lines      -}
listAllCommands st pal
  = intercalate [emptyLine]
  $ map reverse
  $ (listCommandSection st pal <$> commandsList)
 ++ [macroCommandSection st pal]

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
  : [ commandSummary st pal names spec
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
