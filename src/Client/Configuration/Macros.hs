{-# LANGUAGE ApplicativeDo, OverloadedStrings #-}

{-|
Module      : Client.Configuration.Macros
Description : Configuration schema for macros
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}

module Client.Configuration.Macros
  ( macroMapSpec
  , macroCommandSpec
  ) where

import Client.Commands.Interpolation
import Client.Commands.Recognizer (fromCommands, Recognizer)
import Config.Schema.Spec
import Data.Maybe (fromMaybe)
import Data.Text (Text)

macroMapSpec :: ValueSpec (Recognizer Macro)
macroMapSpec = fromCommands <$> listSpec macroValueSpec

macroValueSpec :: ValueSpec (Text, Macro)
macroValueSpec = sectionsSpec "macro" $
  do name     <- reqSection "name" ""
     spec     <- fromMaybe noMacroArguments
             <$> optSection' "arguments" macroArgumentsSpec ""
     commands <- reqSection' "commands" (oneOrList macroCommandSpec) ""
     return (name, Macro name spec commands)

macroArgumentsSpec :: ValueSpec MacroSpec
macroArgumentsSpec = customSpec "macro-arguments" anySpec parseMacroSpecs

macroCommandSpec :: ValueSpec [ExpansionChunk]
macroCommandSpec = customSpec "macro-command" anySpec parseExpansion
