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
  ) where

import           Control.Applicative
import           Data.Text (Text)
import           Data.Attoparsec.Text as P
import           Data.Char

data ExpansionChunk
  = LiteralChunk !Text -- ^ regular text
  | Variable     !Text -- ^ inline variable @$x@ or @${x y}@
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
    , Variable         <$  string "${" <*> P.takeTill (=='}') <* char '}'
    , Variable         <$  char '$' <*> P.takeWhile1 isAlpha
    ]
