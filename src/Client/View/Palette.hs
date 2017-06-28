{-# Language OverloadedStrings #-}
{-|
Module      : Client.View.Palette
Description : View current palette and to see all terminal colors
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

Lines for the @/palette@ command. This view shows all the colors of
the current palette as well as the colors available in the terminal.

-}

module Client.View.Palette
  ( paletteViewLines
  ) where

import           Client.Image.Palette
import           Client.Image.MircFormatting
import           Client.Image.PackedImage
import           Control.Lens
import           Data.List
import           Graphics.Vty.Attributes

digits :: String
digits = "0123456789ABCDEF"

digitImage :: Char -> Image'
digitImage d = string defAttr [' ',d,' ']

decimalImage :: Int -> Image'
decimalImage n
  | n < 10    = string defAttr (' ':'0':show n)
  | otherwise = string defAttr (    ' ':show n)

columns :: [Image'] -> Image'
columns = mconcat . intersperse (char defAttr ' ')

-- | Generate lines used for @/palette@. These lines show
-- all the colors used in the current palette as well as
-- the colors available for use in palettes.
paletteViewLines :: Palette -> [Image']
paletteViewLines pal = reverse $

  [ "Current client palette"
  , ""
  , columns (paletteEntries pal)
  , ""

  , "Current client palette nick highlight colors"
  , ""
  , columns (nickHighlights pal)
  , ""

  , "Chat formatting colors: ^C[foreground[,background]]"
  , ""
  , columns ("   " : map decimalImage [0..15])
  , columns mircColors
  , ""

  , "Available palette colors: 0x<row><col>"
  , ""
  , columns (map digitImage (' ':digits))
  , columns isoColors ]

  ++

  [ columns
  $ digitImage digit
  : [ string (withBackColor defAttr c) "   "
    | col <- [0 .. 15]
    , let c = Color240 (row * 16 + col)
    ]
  | (digit,row) <- zip (drop 1 digits) [0 ..]
  ]


isoColors :: [Image']
isoColors =
  digitImage '0'
  : [ string (withBackColor defAttr (ISOColor c)) "   "
    | c <- [0..15]
    ]

mircColors :: [Image']
mircColors =
  "   "
  : [ string (withBackColor defAttr c) "   "
    | i <- [0..15]
    , let Just c = mircColor i
    ]

paletteEntries :: Palette -> [Image']
paletteEntries pal =
  [ text' (view l pal) name
  | (name, Lens l) <- paletteMap
  ]

nickHighlights :: Palette -> [Image']
nickHighlights pal =
  [ string attr "nicks"
  | attr <- toListOf (palNicks . folded) pal
  ]
