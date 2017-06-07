{-|
Module      : DigraphQuote
Description : Template Haskell quasi-quoter for digraph table
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module DigraphQuote (digraphTable) where

import Data.Char
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Numeric (readHex)

digraphTable :: QuasiQuoter
digraphTable = QuasiQuoter
  { quoteExp  = digraphTableExp
  , quotePat  = const (fail "Digraph table must be an expression")
  , quoteType = const (fail "Digraph table must be an expression")
  , quoteDec  = const (fail "Digraph table must be an expression")
  }

digraphTableExp :: String -> ExpQ
digraphTableExp = stringE . concat <=< traverse parseEntry . lines

-- Parse entries, empty lines are ignored, -- comments are allowed
-- Entries are a two-character digraph followed by a hexadecimal
-- representation of the replacement character's unicode value.
--
-- Examples
--
-- > "'   14
-- > AB 0123
-- > CD 0FDE -- with a comment
-- >
-- > -- with a comment
parseEntry :: String -> Q String
parseEntry line =
  case words line of
    [x,y] : code : rest
       | [(n,"")] <- readHex code
       , isAllowedTerminator rest   -> pure [x,y,chr n]

    rest | isAllowedTerminator rest -> pure "" -- skip empty lines
         | otherwise                -> fail ("Bad digraph entry: " ++ line)

-- Optionally tolerate a comment
isAllowedTerminator :: [String] -> Bool
isAllowedTerminator (('-':'-':_):_) = True
isAllowedTerminator []              = True
isAllowedTerminator _               = False
