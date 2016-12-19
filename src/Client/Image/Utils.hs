{-|
Module      : Client.Image.Utils
Description : Chat message view
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

Provides utilities for formatting Vty Images.
-}

module Client.Image.Utils (lineWrap) where

import           Graphics.Vty.Image

-- | Given an image, break the image up into chunks of at most the
-- given width and stack the resulting chunks vertically top-to-bottom.
lineWrap ::
  Int       {- ^ terminal width       -} ->
  Maybe Int {- ^ optional indentation -} ->
  Image     {- ^ unwrapped image      -} ->
  Image     {- ^ wrapped image        -}
lineWrap w mi img
  | imageWidth img == 0 = emptyImage
  | imageWidth img <= w = terminate w img
  | otherwise =
      terminate w (cropRight w img) <->
      maybe (lineWrapNoIndent w) (lineWrapIndent w) mi
            (cropLeft (imageWidth img - w) img)

-- | Trailing space with default attributes deals with bug in VTY
-- where the formatting will continue past the end of chat messages.
-- This adds an extra space if a line doesn't end on the terminal edge.
terminate ::
  Int   {- ^ terminal width  -} ->
  Image {- ^ unwrapped image -} ->
  Image {- ^ wrapped image   -}
terminate n img
  | imageWidth img == n = img
  | otherwise           = img <|> char defAttr ' '

lineWrapNoIndent ::
  Int   {- ^ terminal width  -} ->
  Image {- ^ unwrapped image -} ->
  Image {- ^ wrapped image   -}
lineWrapNoIndent w img
  | iw <= w   = terminate w img
  | otherwise = cropRight w img <->
                lineWrapNoIndent w (cropLeft (iw - w) img)
  where
    iw = imageWidth img

lineWrapIndent ::
  Int   {- ^ terminal width  -} ->
  Int   {- ^ indentation     -} ->
  Image {- ^ unwrapped image -} ->
  Image {- ^ wrapped image   -}
lineWrapIndent w i img
  | 20 + i >  w = lineWrapNoIndent w img -- ensure we stop wrapping when it doesn't make sense
  | iw + i <= w = terminate w (leftPad i img)
  | otherwise   = leftPad i (cropRight (w-i) img) <->
                  lineWrapIndent w i (cropLeft (iw - w + i) img)
  where
    iw = imageWidth img

leftPad :: Int -> Image -> Image
leftPad i = pad i 0 0 0
