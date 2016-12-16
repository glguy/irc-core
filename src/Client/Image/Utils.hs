{-|
Module      : Client.Image.Utils
Description : Chat message view
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

Provides utilities for formatting Vty Images.
-}

module Client.Image.Utils where

import           Graphics.Vty.Image

-- | Given an image, break the image up into chunks of at most the
-- given width and stack the resulting chunks vertically top-to-bottom.
lineWrap ::
  Int       {- ^ maximum image width -} ->
  Maybe Int {- ^ Indentation of wrapped lines -} ->
  Image     {- ^ unwrapped image     -} ->
  Image     {- ^ wrapped image       -}
lineWrap w mi img
  |imageWidth img > w = cropRight w img <->
                        maybe (lineWrapNoIndent w) (lineWrapIndent w) mi
                              (cropLeft (imageWidth img - w) img)
  | otherwise = img <|> char defAttr ' '


lineWrapNoIndent :: Int -> Image -> Image
lineWrapNoIndent w img
  | imageWidth img > w = cropRight w img <->
                         lineWrapNoIndent w (cropLeft (imageWidth img - w) img)
  | otherwise = img <|> char defAttr ' '
        -- trailing space with default attributes deals with bug in VTY
        -- where the formatting will continue past the end of chat messages

lineWrapIndent :: Int -> Int -> Image -> Image
lineWrapIndent w i img
      | imageWidth img > w - i = pad i 0 0 0 (cropRight (w-i) img) <->
                                lineWrapIndent w i (cropLeft (imageWidth img - w + i) img)
      | otherwise = pad i 0 0 0 img <|> char defAttr ' '
