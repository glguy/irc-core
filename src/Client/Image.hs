{-# Language BangPatterns #-}
{-|
Module      : Client.Image
Description : UI renderer
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the renderer for the client's UI.

-}
module Client.Image (clientPicture) where

import           Client.Image.Palette
import           Client.Image.StatusLine
import           Client.Image.Textbox
import           Client.State
import           Client.State.Focus
import           Client.View
import           Control.Lens
import           Data.List
import           Graphics.Vty (Background(..), Picture(..), Cursor(..))
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

clientImage ::
  ClientState ->
  (Int, Image, ClientState) -- ^ text box cursor position, image, updated state
clientImage st = (pos, img, st'')
  where
    (st', mp) = messagePane mainHeight focus (view clientSubfocus st) st
    (st'', extras) = mapAccumL renderExtra st' splits

    (pos, tbImg) = textboxImage st''
    img = vertCat extras <->
          mp <->
          statusLineImage st'' <->
          tbImg

    focus = view clientFocus st

    (mainHeight, splitHeight) = clientWindowHeights st
    splits                    = clientExtraFocuses st

    renderExtra stIn focus1 = (stOut, outImg)
      where
        (stOut,msgImg) = messagePane splitHeight focus1 FocusMessages stIn
        pal = clientPalette st
        divider = view palWindowDivider pal
        outImg = msgImg <-> minorStatusLineImage focus1 st
                        <-> charFill divider ' ' (view clientWidth st) 1

messagePane :: Int -> Focus -> Subfocus -> ClientState -> (ClientState, Image)
messagePane h focus subfocus st = (st', img)
  where
    images = viewLines focus subfocus st
    vimg   = assemble emptyImage images
    vimg1  = cropBottom h vimg
    img    = pad 0 (h - imageHeight vimg1) 0 0 vimg1

    overscroll = vh - imageHeight vimg

    st' = over clientScroll (max 0 . subtract overscroll) st

    assemble acc _ | imageHeight acc >= vh = cropTop vh acc
    assemble acc [] = acc
    assemble acc (x:xs) = assemble (lineWrap w x <-> acc) xs

    scroll = view clientScroll st
    vh     = h + scroll

    w      = view clientWidth st

lineWrap :: Int -> Image -> Image
lineWrap w img
  | imageWidth img > w = cropRight w img <-> lineWrap w (cropLeft (imageWidth img - w) img)
  | otherwise = img <|> char defAttr ' '
                        -- trailing space with default attributes deals with bug in VTY
                        -- where the formatting will continue past the end of chat messages
