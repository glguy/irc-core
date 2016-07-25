{-|
Module      : Client.IdentifierColors
Description : Mapping from identifiers to console colors
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the color mapping for nick highlighting.

-}

module Client.IdentifierColors (identifierColor) where

import Graphics.Vty.Image
import Irc.Identifier
import Data.Array
import qualified Data.ByteString as B

-- | Compute a color from the denotation of an identifier.
-- This color will be consistent for different capitalizations
-- and will be consistent across program executions.
identifierColor :: Identifier -> Color
identifierColor ident = nickColorPalette ! i
  where
    hash = hashIdentity ident
    (0,hi) = bounds nickColorPalette
    n = hi+1
    i = hash`mod`n

hashIdentity :: Identifier -> Int
hashIdentity ident =
    let h1 = B.foldl' (\acc b -> fromIntegral b + 33 * acc) 0 (idDenote ident)
    in h1 + (h1 `quot` 32)

nickColorPalette :: Array Int Color
nickColorPalette = listArray (0, length xs - 1) xs
  where
    xs = [cyan, magenta, green, yellow, blue,
          brightCyan, brightMagenta, brightGreen, brightBlue]
