{-# Language OverloadedStrings, GADTs, RankNTypes #-}

{-|
Module      : Client.Commands.Interpolation
Description : Parser and evaluator for string interpolation in commands
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module is able to parse commands with inline variables and then
to evaluate those variables to produce a complete command that varies
by the current context.

Variables are built from 1 or more letters.

Optional arguments are suffixed with a @?@

Remaining text arguments are suffixed with a @*@

-}
module Client.Commands.Interpolation
  ( ExpansionChunk(..)
  , parseExpansion
  , resolveMacroExpansions
  , Macro(..)
  , MacroSpec(..)
  , parseMacroSpecs
  , noMacroArguments
  ) where

import Client.Commands.Arguments.Spec (optionalArg, remainingArg, simpleToken, Args)
import Control.Applicative (Alternative, liftA2, (<|>), many, optional)
import Data.Attoparsec.Text as P
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

-- | Parsed chunk of an expandable command
data ExpansionChunk
  -- | regular text
  = LiteralChunk Text
  -- | inline variable @$x@ or @${x y}@
  | VariableChunk Text
  -- | inline variable @$1@ or @${1}@
  | IntegerChunk Integer
  -- | bracketed variable with default @${x|lit}@
  | DefaultChunk ExpansionChunk Text
  deriving Show

data Macro
  = Macro
  { macroName :: Text
  , macroSpec :: MacroSpec
  , macroCommands :: [[ExpansionChunk]]
  } deriving Show

data MacroSpec where
  MacroSpec :: (forall r. Args r [String]) -> MacroSpec

instance Show MacroSpec where
  show MacroSpec{} = "MacroSpec"

-- | Specification used when unspecified, no arguments.
noMacroArguments :: MacroSpec
noMacroArguments = MacroSpec (pure [])

parseMacroSpecs :: Text -> Either Text MacroSpec
parseMacroSpecs txt =
  case parseOnly (macroSpecs <* endOfInput) txt of
    Left e     -> Left (Text.pack e)
    Right spec -> Right spec

macroSpecs :: Parser MacroSpec
macroSpecs =
  do var <- P.takeWhile1 isAlpha
     mode <- optional (True <$ char '?' <|> False <$ char '*')
     P.skipSpace
     case mode of
       Nothing    -> addReq var <$> macroSpecs
       Just True  -> addOpt var <$> macroSpecs
       Just False -> MacroSpec (pure <$> remainingArg (Text.unpack var)) <$ P.endOfInput
  <|> noMacroArguments <$ P.endOfInput
  where
    add1 desc = liftA2 (:) (simpleToken (Text.unpack desc))
    addBrackets desc = "[" <> desc <> "]"

    addOpt var (MacroSpec rest) = MacroSpec (fromMaybe [] <$> optionalArg (add1 (addBrackets var) rest))
    addReq var (MacroSpec rest) = MacroSpec (add1 var rest)

-- | Parse a 'Text' searching for the expansions as specified in
-- 'ExpansionChunk'. @$$@ is used to escape a single @$@.
parseExpansion :: Text -> Either Text [ExpansionChunk]
parseExpansion txt =
  case parseOnly (many parseChunk <* endOfInput) txt of
    Left e       -> Left (Text.pack e)
    Right chunks -> Right chunks

parseChunk :: Parser ExpansionChunk
parseChunk =
  choice
    [ LiteralChunk     <$> P.takeWhile1 (/= '$')
    , LiteralChunk "$" <$  P.string "$$"
    , string "${" *> parseDefaulted <* char '}'
    , char '$' *> parseVariable
    ]

parseDefaulted :: Parser ExpansionChunk
parseDefaulted =
  construct
    <$> parseVariable
    <*> optional (char '|' *> P.takeWhile (/= '}'))
 where
 construct ch Nothing  = ch
 construct ch (Just l) = DefaultChunk ch l

parseVariable :: Parser ExpansionChunk
parseVariable = IntegerChunk  <$> P.decimal
            <|> VariableChunk <$> P.takeWhile1 isAlpha

-- | Attempt to expand all of the elements in the given list using
-- the two expansion functions. If the expansion of any chunk
-- fails the whole expansion fails.
resolveMacroExpansions ::
  Alternative f =>
  (Text    -> f Text) {- ^ variable resolution           -} ->
  (Integer -> f Text) {- ^ argument index resolution     -} ->
  [ExpansionChunk]    {- ^ chunks                        -} ->
  f Text              {- ^ concatenated, expanded chunks -}
resolveMacroExpansions var arg xs = Text.concat <$> traverse resolve1 xs
  where
    resolve1 (LiteralChunk lit) = pure lit
    resolve1 (VariableChunk v)  = var v
    resolve1 (IntegerChunk i)   = arg i
    resolve1 (DefaultChunk p d) = resolve1 p <|> pure d
