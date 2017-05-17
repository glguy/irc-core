{-|
Module      : Client.Image.LineWrap
Description : Chat message view
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

Provides utilities for line wrapping images.
-}

module Client.Image.LineWrap (lineWrap, lineWrapChat) where

import           Client.Image.PackedImage
import           Data.Semigroup
import qualified Graphics.Vty.Image as Vty
import           Graphics.Vty.Attributes
import qualified Data.Text.Lazy as L

-- | Given an image, break the image up into chunks of at most the
-- given width and stack the resulting chunks vertically top-to-bottom.
lineWrap ::
  Int       {- ^ terminal width       -} ->
  Vty.Image {- ^ unwrapped image      -} ->
  Vty.Image {- ^ wrapped image        -}
lineWrap w img
  | Vty.imageWidth img == 0 = Vty.emptyImage
  | Vty.imageWidth img <= w = terminate w img
  | otherwise =
      terminate w (Vty.cropRight w img) Vty.<->
      lineWrap w (Vty.cropLeft (Vty.imageWidth img - w) img)

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


lineWrapChat ::
  Int       {- ^ terminal width       -} ->
  Maybe Int {- ^ optional indentation -} ->
  Image'    {- ^ unwrapped image      -} ->
  [Image']  {- ^ wrapped image        -}
lineWrapChat w (Just i)
  | 2*i <= w = reverse . addPadding i . wordLineWrap w (w-i)
lineWrapChat w _  = reverse . wordLineWrap w w


addPadding :: Int -> [Image'] -> [Image']
addPadding _ [] = []
addPadding i (x:xs) = x : map indent xs
  where indent = (string defAttr (replicate i ' ') <>)


wordLineWrap ::
  Int      {- ^ first line length     -} ->
  Int      {- ^ secondary line length -} ->
  Image'   {- ^ image                 -} ->
  [Image'] {- ^ splits                -}
wordLineWrap w wNext img
  | imgW == 0 = []
  | imgW <= w = [img]
  | otherwise = l : wordLineWrap wNext wNext (dropSpaces r)
  where
    imgW = imageWidth img
    x:xs = splitOptions img

    (l,r) = splitImage width img

    width
      | x <= w = go x xs
      | otherwise = w

    go y [] = min y w
    go y (z:zs)
      | z-y > wNext = w
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
