{-|
Module      : Client.IdentifierColors
Description : Mapping from identifiers to console colors
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the color mapping for nick highlighting.

-}

module Client.IdentifierColors
  ( identifierColor
  ) where

import           Client.Image.Palette
import qualified Data.ByteString as B
import qualified Data.Vector as Vector
import           Irc.Identifier
import           Graphics.Vty.Image

-- | Compute a color from the denotation of an identifier.
-- This color will be consistent for different capitalizations
-- and will be consistent across program executions.
identifierColor :: Palette -> Identifier -> Color
identifierColor palette ident = v Vector.! i
  where
    v = palNicks palette
    i = hashIdentity ident `mod` Vector.length v

hashIdentity :: Identifier -> Int
hashIdentity ident =
    let h1 = B.foldl' (\acc b -> fromIntegral b + 33 * acc) 0 (idDenote ident)
    in h1 + (h1 `quot` 32)
