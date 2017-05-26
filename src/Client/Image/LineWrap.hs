{-|
Module      : Client.Image.LineWrap
Description : Chat message view
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

Provides utilities for line wrapping images.
-}

module Client.Image.LineWrap
  ( lineWrap
  , lineWrapPrefix
  , terminate
  ) where

import           Client.Image.PackedImage
import           Data.Semigroup
import qualified Graphics.Vty.Image as Vty
import           Graphics.Vty.Attributes
import qualified Data.Text.Lazy as L


-- | Trailing space with default attributes deals with bug in VTY
-- where the formatting will continue past the end of chat messages.
-- This adds an extra space if a line doesn't end on the terminal edge.
terminate ::
  Int       {- ^ terminal width  -} ->
  Vty.Image {- ^ unwrapped image -} ->
  Vty.Image {- ^ wrapped image   -}
terminate n img
  | Vty.imageWidth img == n = img
  | otherwise               = img Vty.<|> Vty.char defAttr ' '


lineWrapPrefix ::
  Int       {- ^ terminal width  -} ->
  Image'    {- ^ prefix image    -} ->
  Image'    {- ^ unwrapped image -} ->
  [Image']  {- ^ wrapped image   -}
lineWrapPrefix w pfx img
  | 3*pfxW <= w = pfx <> char defAttr ' ' <> x :
                  map (pad<>) xs
  where
    pfxW = imageWidth pfx
    x:xs = lineWrap (w - pfxW - 1) img
    pad  = string defAttr (replicate (pfxW + 1) ' ')

-- Don't index when the window is tiny
lineWrapPrefix w pfx img = lineWrap w (pfx <> char defAttr ' ' <> img)


lineWrap ::
  Int      {- ^ first line length     -} ->
  Image'   {- ^ image                 -} ->
  [Image'] {- ^ splits                -}
lineWrap w img
  | imageWidth img <= w = [img] -- could be empty
  | otherwise           = lineWrap' w img


lineWrap' ::
  Int      {- ^ first line length     -} ->
  Image'   {- ^ image                 -} ->
  [Image'] {- ^ splits                -}
lineWrap' w img
  | imgW == 0 = []
  | imgW <= w = [img]
  | otherwise = l : lineWrap' w (dropSpaces r)
  where
    imgW = imageWidth img
    x:xs = splitOptions img

    (l,r) = splitImage width img

    width
      | x <= w = go x xs
      | otherwise = w

    go y [] = min y w
    go y (z:zs)
      | z-y > w = w
      | z > w = y
      | otherwise = go z zs


-- | List of image widths suitable for breaking the image on
-- that correspond to word breaks.
splitOptions :: Image' -> [Int]
splitOptions
  = dropWhile (0==)
  . scanl1 (\x y -> 1 + x + y)
  . map (Vty.wcswidth . L.unpack)
  . L.split (' '==)
  . imageText


-- | Drop the leading spaces from an image
dropSpaces :: Image' -> Image'
dropSpaces img
  | n == 0    = img
  | otherwise = snd (splitImage n img)
  where
    n = fromIntegral $ L.length $ L.takeWhile (' '==) $ imageText img
