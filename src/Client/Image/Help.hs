{-|
Module      : Client.Image.Help
Description : Renderer for help lines
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the rendering used for the @/help@ command.

-}
module Client.Image.Help
  ( helpImageLines
  ) where

import           Client.Image.Arguments
import           Client.Image.Palette
import           Client.Image.MircFormatting
import           Client.Commands
import           Control.Lens
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Graphics.Vty.Image

helpImageLines :: Maybe Text -> Palette -> [Image]
helpImageLines mbCmd pal =
  case mbCmd of
    Nothing  -> listAllCommands pal
    Just cmd -> commandHelpLines cmd pal

commandHelpLines :: Text -> Palette -> [Image]
commandHelpLines cmdName pal =
  case view (at cmdName) commands of
    Nothing -> [string (view palError pal) "Unknown command, try /help"]
    Just cmd@(Command args doc impl) ->
      reverse $ commandSummary pal (pure cmdName) cmd
              : emptyImage
              : explainContext impl
              : emptyImage
              : map parseIrcText docs
      where
        docs = Text.lines doc

explainContext :: CommandImpl a -> Image
explainContext impl =
  case impl of
    ClientCommand{}  -> go "client command" "works everywhere"
    NetworkCommand{} -> go "network command" "works when focused on active network"
    ChannelCommand{} -> go "channel command" "works when focused on active channel"
    ChatCommand{}    -> go "chat command" "works when focused on an active channel or private message"
  where
    go x y = string (withStyle defAttr bold) x <|>
             string defAttr (": " ++ y)

listAllCommands :: Palette -> [Image]
listAllCommands pal = reverse (map (uncurry (commandSummary pal)) commandsList)

commandSummary :: Palette -> NonEmpty Text -> Command -> Image
commandSummary pal (cmd :| _) (Command args _doc impl) =
  char defAttr '/' <|>
  text' (view palCommand pal) cmd <|>
  argumentsImage pal' args ""

  where
    pal' = set palCommandPlaceholder defAttr pal
