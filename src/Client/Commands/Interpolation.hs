{-# Language OverloadedStrings #-}

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
  , resolveExpansions
  ) where

import           Control.Applicative
import           Data.Attoparsec.Text as P
import           Data.Char
import qualified Data.Text as Text
import           Data.Text (Text)

data ExpansionChunk
  = LiteralChunk Text -- ^ regular text
  | VariableChunk Text -- ^ inline variable @$x@ or @${x y}@
  | IntegerChunk Integer -- ^ inline variable @$1@ or @${1}@
  deriving Show

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
    , string "${" *> parseVariable <* char '}'
    , char '$' *> parseVariable
    ]

parseVariable :: Parser ExpansionChunk
parseVariable = IntegerChunk  <$> P.decimal
            <|> VariableChunk <$> P.takeWhile1 isAlpha

resolveExpansions ::
  (Text    -> Maybe Text) {- ^ variable resolution       -} ->
  (Integer -> Maybe Text) {- ^ argument index resolution -} ->
  [ExpansionChunk]                                          ->
  Maybe Text
resolveExpansions var arg xs = Text.concat <$> traverse resolve1 xs
  where
    resolve1 (LiteralChunk lit) = Just lit
    resolve1 (VariableChunk v)  = var v
    resolve1 (IntegerChunk i)   = arg i
