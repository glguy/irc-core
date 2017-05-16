{-|
Module      : Client.Image.Utils
Description : Chat message view
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

Provides utilities for formatting Vty Images.
-}

module Client.Image.Utils (lineWrap, lineWrapChat) where

import           Client.Image.PackedImage
import           Data.Semigroup
import qualified Graphics.Vty.Image as Vty
import           Graphics.Vty.Attributes

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


-- | Given an image, break the image up into chunks of at most the
-- given width and stack the resulting chunks vertically top-to-bottom.
lineWrapChat ::
  Int       {- ^ terminal width       -} ->
  Maybe Int {- ^ optional indentation -} ->
  Image'    {- ^ unwrapped image      -} ->
  [Image']  {- ^ wrapped image        -}
lineWrapChat w mi img
  | imageWidth img == 0 = []
  | imageWidth img <= w = [img]
  | otherwise =
      case mi of
        Just i | 2*i <= w ->
           reverse (map indent (simpleLineWrap (w-i) r)) ++ [l]
          where indent = (string defAttr (replicate i ' ') <>)
        _ -> reverse (simpleLineWrap w r) ++ [l]
  where
    (l,r) = splitImage w img

simpleLineWrap ::
  Int      {- ^ terminal width  -} ->
  Image'   {- ^ unwrapped image -} ->
  [Image'] {- ^ wrapped image   -}
simpleLineWrap w img
  | iw <= w = [img]
  | otherwise = l : simpleLineWrap w r
  where
    iw = imageWidth img
    (l,r) = splitImage w img
