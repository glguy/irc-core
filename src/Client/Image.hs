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

import           Client.Configuration (LayoutMode(..))
import           Client.Image.Layout
import           Client.Image.StatusLine
import           Client.Image.Textbox
import           Client.State
import           Client.State.Focus
import           Client.View
import           Control.Lens
import           Graphics.Vty            (Background (..), Cursor (..),
                                          Picture (..))
import           Graphics.Vty.Image


-- | Generate a 'Picture' for the current client state. The resulting
-- client state is updated for render specific information like scrolling.
clientPicture :: ClientState -> (Picture, ClientState)
clientPicture st = (pic, st')
    where
      (pos, img, st') = clientImage st
      pic = Picture
              { picCursor     = AbsoluteCursor pos (view clientHeight st - 1)
              , picBackground = ClearBackground
              , picLayers     = [img]
              }

-- | Primary UI render logic
clientImage ::
  ClientState               {- ^ client state -} ->
  (Int, Image, ClientState) {- ^ text box cursor position, image, updated state -}
clientImage st = (pos, img, st')
  where
    focus                     = view clientFocus st
    (pos , nextOffset, tbImg) = textboxImage st

    -- update client state for scroll clamp
    !st' = set clientTextBoxOffset nextOffset
         $ over clientScroll (max 0 . subtract overscroll) st

    (overscroll, msgs) = drawLayout st msgHeight mainLines extraLines

    msgHeight = max 0 (view clientHeight st - imageHeight bottomImg)

    img       = msgs <-> bottomImg
    bottomImg = statusLineImage st <-> tbImg

    mainLines  = viewLines focus (view clientSubfocus st) st
    extraLines = [ (x, viewLines x FocusMessages st)
                 | x <- clientExtraFocuses st ]
