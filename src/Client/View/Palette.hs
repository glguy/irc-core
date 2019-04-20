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
import           Numeric (showHex)

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
  , "Available terminal palette colors (hex)"
  , ""
  ] ++
  terminalColorTable

terminalColorTable :: [Image']
terminalColorTable =
  isoColors :
  "" : colorBox 0x10 ++
  "" : colorBox 0x7c ++
  "" : "   " <> foldMap (\c -> colorBlock showPadHex c (Color240 (fromIntegral (c-16)))) [0xe8 .. 0xf3]
     : "   " <> foldMap (\c -> colorBlock showPadHex c (Color240 (fromIntegral (c-16)))) [0xf4 .. 0xff]
     : []

colorBox :: Int -> [Image']
colorBox start =
  [ "   " <>
     columns
      [ mconcat
          [ colorBlock showPadHex k (Color240 (fromIntegral (k - 16)))
          | k <- [j, j+6 .. j + 30 ] ]
      | j <- [i, i + 0x24, i + 0x48 ]
      ]
  | i <- [ start .. start + 5 ]
  ]

isLight :: Color -> Bool
isLight (ISOColor c) = c `elem` [7, 10, 11, 14, 15]
isLight (Color240 c) =
  case color240CodeToRGB c of
    Just (r, g, b) -> (r `max` g `max` b) > 200
    Nothing        -> True


isoColors :: Image'
isoColors = "   " <> foldMap (\c -> colorBlock showPadHex c (ISOColor (fromIntegral c))) [0 .. 15]

colorTable :: [Image']
colorTable
  = map (\imgs -> mconcat ("   " : imgs))
  $ chunksOf 8 [ colorBlock showPadDec i (mircColors Vector.! i) | i <- [0 .. 15] ]
  ++ [[]]
  ++ chunksOf 12 [ colorBlock showPadDec i (mircColors Vector.! i) | i <- [16 .. 98] ]

colorBlock :: (Int -> String) -> Int -> Color -> Image'
colorBlock showNum i c =
  string (withForeColor (withBackColor defAttr c) (if isLight c then black else white)) (showNum i)

showPadDec :: Int -> String
showPadDec i
  | i < 10    = ' ' : '0' : shows i " "
  | otherwise = ' ' :       shows i " "

showPadHex :: Int -> String
showPadHex i
  | i < 16    = ' ' : '0' : showHex i " "
  | otherwise = ' ' :       showHex i " "

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
