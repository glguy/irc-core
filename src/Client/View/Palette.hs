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
import           Data.List.Split (chunksOf)
import           Graphics.Vty.Attributes
import qualified Data.Vector as Vector

digits :: String
digits = "0123456789ABCDEF"

digitImage :: Char -> Image'
digitImage d = string defAttr [' ', d, ' ']

columns :: [Image'] -> Image'
columns = mconcat . intersperse (char defAttr ' ')

-- | Generate lines used for @/palette@. These lines show
-- all the colors used in the current palette as well as
-- the colors available for use in palettes.
paletteViewLines :: Palette -> [Image']
paletteViewLines pal = reverse $

  [ "Current client palette:"
  , ""
  , columns (paletteEntries pal)
  , ""

  , "Current client palette nick highlight colors:"
  , ""
  , columns (nickHighlights pal)
  , ""

  , "Chat formatting modes:"
  , ""
  , "   C-b  C-_       C-]    C-v     C-o"
  , parseIrcText "   \^Bbold\^B \^_underline\^_ \^]italic\^] \^Vreverse\^V reset"
  , ""

  , "Chat formatting colors: C-c[foreground[,background]]"
  , ""
  ] ++

  colorTable

  ++
  [ ""
  , "Available terminal palette colors: 0x<row><col>"
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

isLight :: Color -> Bool
isLight (ISOColor c) = c `elem` [1, 3, 5, 6, 7]
isLight (Color240 c) =
  case color240CodeToRGB c of
    Just (r, g, b) -> (r `max` g `max` b) > 200
    Nothing        -> True


isoColors :: [Image']
isoColors =
  digitImage '0'
  : [ string (withBackColor defAttr (ISOColor c)) "   "
    | c <- [0..15]
    ]

colorTable :: [Image']
colorTable
  = map (\imgs -> mconcat ("   " : imgs))
  $ chunksOf 8 [ render i (mircColors Vector.! i) | i <- [0 .. 15] ]
  ++ [[]]
  ++ chunksOf 12 [ render i (mircColors Vector.! i) | i <- [16 .. 98] ]
  where
    showPad i
      | i < 10    = '0' : show i
      | otherwise = show i

    render i c =
      string (withForeColor (withBackColor defAttr c) (if isLight c then black else white)) (' ' : showPad i ++ " ")

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
