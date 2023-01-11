{-# LANGUAGE BangPatterns #-}
{-|
Module      : Client.Image
Description : UI renderer
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the renderer for the client's UI.

-}
module Client.Image (clientPicture) where

import Client.Image.Layout (drawLayout)
import Client.State (clientHeight, clientScroll, clientTextBoxOffset, ClientState)
import Control.Lens (view, over, set)
import Graphics.Vty (Background (..), Cursor (..),  Picture (..))
import Graphics.Vty.Image (Image)

-- | Generate a 'Picture' for the current client state. The resulting
-- client state is updated for render specific information like scrolling.
clientPicture :: ClientState -> (Picture, ClientState)
clientPicture st = (pic, st')
    where
      (row, col, img, st') = clientImage st
      pic = Picture
              { picCursor     = AbsoluteCursor col (view clientHeight st - row)
              , picBackground = ClearBackground
              , picLayers     = [img]
              }

-- | Primary UI render logic
clientImage ::
  ClientState               {- ^ client state -} ->
  (Int, Int, Image, ClientState) {- ^ cursor row, cursor col, image, updated state -}
clientImage st = (row, col, img, st')
  where
    -- update client state for scroll clamp
    !st' = set clientTextBoxOffset nextOffset
         $ over clientScroll (max 0 . subtract overscroll) st

    (overscroll, row, col, nextOffset, img) = drawLayout st
