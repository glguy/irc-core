{-|
Module      : Client.Image.PaletteView
Description : View current palette and to see all terminal colors
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

-}

module Client.Image.PaletteView
  ( paletteViewLines
  ) where

import           Client.Image.Palette
import           Control.Lens
import           Data.List
import           Graphics.Vty.Image

digits :: String
digits = "0123456789ABCDEF"

digitImage :: Char -> Image
digitImage d = string defAttr [' ',d,' ']

columns = horizCat . intersperse (char defAttr ' ')

paletteViewLines :: Palette -> [Image]
paletteViewLines pal =
  [ columns
  $ digitImage digit
  : [ string (withBackColor defAttr c) "   "
    | col <- [0 .. 15]
    , let c = Color240 (row * 16 + col)
    ]
  | (digit,row) <- reverse $ take 15 $ zip (drop 1 digits) [0 ..]
  ] ++
  [ columns
  $ digitImage '0'
  : [ string (withBackColor defAttr (ISOColor c)) "   "
    | c <- [0..15]
    ]

  , columns (map digitImage (' ':digits))
  , emptyImage
  , columns
    [ text' (view l pal) name
    | (name, Lens l) <- paletteMap
    ]
  , emptyImage
  , columns
    [ string attr "nicks"
    | attr <- toListOf (palNicks . folded) pal
    ]
  ]
