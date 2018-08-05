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

import           Control.Applicative
import           Data.Attoparsec.Text as P
import           Data.Char
import           Data.Maybe
import qualified Data.Text as Text
import           Data.Text (Text)

import           Client.Commands.Arguments.Spec

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
  { macroSpec :: MacroSpec
  , macroCommands :: [[ExpansionChunk]]
  } deriving Show

data MacroSpec where
  MacroSpec :: (forall r. Args r [String]) -> MacroSpec

instance Show MacroSpec where
  show MacroSpec{} = "MacroSpec"

-- | Specification used when unspecified, no arguments.
noMacroArguments :: MacroSpec
noMacroArguments = MacroSpec (pure [])

parseMacroSpecs :: Text -> Maybe MacroSpec
parseMacroSpecs txt =
  case parseOnly (macroSpecs <* endOfInput) txt of
    Left{}     -> Nothing
    Right spec -> Just spec

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

    addOpt var (MacroSpec rest) = MacroSpec (fromMaybe [] <$> optionalArg (add1 var rest))
    addReq var (MacroSpec rest) = MacroSpec (add1 var rest)

-- | Parse a 'Text' searching for the expansions as specified in
-- 'ExpansionChunk'. @$$@ is used to escape a single @$@.
parseExpansion :: Text -> Maybe [ExpansionChunk]
parseExpansion txt =
  case parseOnly (many parseChunk <* endOfInput) txt of
    Left{}       -> Nothing
    Right chunks -> Just chunks

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
    <*> optional (char '|' *> P.takeWhile1 (/= '}'))
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
