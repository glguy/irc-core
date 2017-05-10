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

import           Config.Schema.Spec
import           Client.Commands.Interpolation
import           Client.Commands.Recognizer
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)

macroMapSpec :: ValueSpecs (Recognizer Macro)
macroMapSpec = fromCommands <$> listSpec macroValueSpecs

macroValueSpecs :: ValueSpecs (Text, Macro)
macroValueSpecs = sectionsSpec "macro" $
  do name     <- reqSection "name" ""
     spec     <- fromMaybe noMacroArguments
             <$> optSection' "arguments" macroArgumentsSpec ""
     commands <- reqSection' "commands" (listSpec macroCommandSpec) ""
     return (name, Macro spec commands)

macroArgumentsSpec :: ValueSpecs MacroSpec
macroArgumentsSpec = customSpec "macro arguments" valuesSpec parseMacroSpecs

macroCommandSpec :: ValueSpecs [ExpansionChunk]
macroCommandSpec = customSpec "macro command" valuesSpec parseExpansion
