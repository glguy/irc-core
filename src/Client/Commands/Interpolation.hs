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
import qualified Data.Text as Text
import           Data.Text (Text)

import           Client.Commands.Arguments

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
  MacroSpec :: (forall r. ArgumentSpec r s) -> MacroSpec

instance Show MacroSpec where
  showsPrec p (MacroSpec as)
    = showParen (p >= 11)
    $ showString "MacroSpec " . showsPrec 11 as

-- | Specification used when unspecified, no arguments.
noMacroArguments :: MacroSpec
noMacroArguments = MacroSpec NoArg

parseMacroSpecs :: Text -> Maybe MacroSpec
parseMacroSpecs txt =
  case parseOnly (macroSpecs <* endOfInput) txt of
    Left{}     -> Nothing
    Right spec -> Just spec

macroSpecs :: Parser MacroSpec
macroSpecs =
  cons <$> P.takeWhile1 isAlpha
       <*> optional (char '?')
       <*  P.skipSpace
       <*> macroSpecs
    <|> pure (MacroSpec NoArg)
 where
 cons desc (Just _) (MacroSpec rest) = MacroSpec (OptTokenArg (Text.unpack desc) rest)
 cons desc Nothing  (MacroSpec rest) = MacroSpec (ReqTokenArg (Text.unpack desc) rest)

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
  (Text    -> Maybe Text) {- ^ variable resolution       -} ->
  (Integer -> Maybe Text) {- ^ argument index resolution -} ->
  [ExpansionChunk]        {- ^ chunks                    -} ->
  Maybe Text              {- ^ concatenated, expanded chunks -}
resolveMacroExpansions var arg xs = Text.concat <$> traverse resolve1 xs
  where
    resolve1 (LiteralChunk lit) = Just lit
    resolve1 (VariableChunk v)  = var v
    resolve1 (IntegerChunk i)   = arg i
    resolve1 (DefaultChunk p d) = resolve1 p <|> Just d
