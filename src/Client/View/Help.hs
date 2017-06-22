{-# Language BangPatterns, OverloadedStrings #-}

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

import           Client.Commands
import           Client.Commands.Arguments
import           Client.Commands.Recognizer
import           Client.Image.Arguments
import           Client.Image.MircFormatting
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Control.Lens
import           Data.Foldable (toList)
import           Data.List (delete, intercalate)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Graphics.Vty.Attributes

-- | Generate either the list of all commands and their arguments,
-- or when given a command name generate the detailed help text
-- for that command.
helpImageLines ::
  Maybe Text {- ^ optional command name -} ->
  Palette    {- ^ palette               -} ->
  [Image']   {- ^ help lines            -}
helpImageLines mbCmd pal =
  case mbCmd of
    Nothing  -> listAllCommands pal
    Just cmd -> commandHelpLines cmd pal

-- | Generate detailed help lines for the command with the given name.
commandHelpLines ::
  Text        {- ^ command name -} ->
  Palette     {- ^ palette      -} ->
  [Image']    {- ^ lines        -}
commandHelpLines cmdName pal =
  case recognize cmdName commands of
    Invalid -> [string (view palError pal) "Unknown command, try /help"]
    Prefix sfxs ->
      [string (view palError pal) $ "Unknown command, did you mean: " ++ suggestions]
      where
      suggestions = Text.unpack $ Text.intercalate " " ((cmdName <>) <$> sfxs)
    Exact Command{cmdNames = names, cmdImplementation = impl,
                  cmdArgumentSpec = spec, cmdDocumentation = doc} ->
      reverse $ heading "Syntax: " <> commandSummary pal (pure cmdName) spec
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
  Palette  {- ^ palette    -} ->
  [Image'] {- ^ help lines -}
listAllCommands pal
  = intercalate [emptyLine]
  $ map reverse
  $ listCommandSection pal <$> commandsList

listCommandSection ::
  Palette        {- ^ palette         -} ->
  CommandSection {- ^ command section -} ->
  [Image']       {- ^ help lines      -}
listCommandSection pal sec
  = text' (withStyle defAttr bold) (cmdSectionName sec)
  : [ commandSummary pal names spec
    | -- pattern needed due to existential quantification
      Command { cmdNames        = names
              , cmdArgumentSpec = spec
              } <- cmdSectionCmds sec
    ]

-- | Generate the help line for the given command and its
-- specification for use in the list of commands.
commandSummary ::
  Palette          {- ^ palette                  -} ->
  NonEmpty Text    {- ^ command name and aliases -} ->
  ArgumentSpec r a {- ^ argument specification   -} ->
  Image'           {- ^ summary help line        -}
commandSummary pal (cmd :| _) args  =
  char defAttr '/' <>
  text' (view palCommandReady pal) cmd <>
  argumentsImage () pal' (forgetFormats args) {-draw-placeholders=-}True ""

  where
    pal' = set palCommandPlaceholder defAttr pal

-- Empty line used as a separator
emptyLine :: Image'
emptyLine = char defAttr ' '
