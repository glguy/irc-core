{-# LANGUAGE BangPatterns #-}
{-|
Module      : Client.Image
Description : UI renderer
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the renderer for the client's UI.

-}
module Client.Image
  ( clientPicture
  , scrollAmount
  ) where

import           Client.Image.Palette
import           Client.Image.StatusLine
import           Client.Image.Textbox
import           Client.Image.Utils
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
    (mainHeight, splitHeight) = clientWindowHeights (imageHeight statusLine) st
    splitFocuses              = clientExtraFocuses st
    focus                     = view clientFocus st
    (pos , nextOffset, tbImg) = textboxImage st

    -- update client state for scroll clamp
    !st' = set clientTextBoxOffset nextOffset
         $ over clientScroll (max 0 . subtract overscroll) st

    (overscroll, msgs) = messagePane mainHeight focus (view clientSubfocus st) st
    splits = renderExtra st' <$> splitFocuses
    -- outgoing state is ignored here, splits don't get to truncate scrollback

    img = vertCat splits      <->
          msgs                <->
          statusLine          <->
          tbImg

    statusLine = statusLineImage st

    renderExtra stIn focus1 = outImg
      where
        (_,msgImg) = messagePane splitHeight focus1 FocusMessages stIn
        pal = clientPalette stIn
        divider = view palWindowDivider pal
        outImg = msgImg <-> minorStatusLineImage focus1 stIn
                        <-> charFill divider ' ' (view clientWidth stIn) 1

-- | Generate an image corresponding to the image lines of the given
-- focus and subfocus. Returns the number of lines overscrolled to
-- assist in clamping scroll to the lines available in the window.
messagePane ::
  Int          {- ^ available rows                -} ->
  Focus        {- ^ focused window                -} ->
  Subfocus     {- ^ subfocus to render            -} ->
  ClientState  {- ^ client state                  -} ->
  (Int, Image) {- ^ overscroll, rendered messages -}
messagePane h focus subfocus st = (overscroll, img)
  where
    images = viewLines focus subfocus st
    vimg   = assemble emptyImage images
    vimg1  = cropBottom h vimg
    img    = pad 0 (h - imageHeight vimg1) 0 0 vimg1

    overscroll = vh - imageHeight vimg

    assemble acc _ | imageHeight acc >= vh = cropTop vh acc
    assemble acc [] = acc
    assemble acc (x:xs) = assemble (lineWrap w Nothing x <-> acc) xs

    scroll = view clientScroll st
    vh     = h + scroll

    w      = view clientWidth st


-- | Compute the number of lines in a page at the current window size
scrollAmount ::
  ClientState {- ^ client state              -} ->
  Int         {- ^ scroll amount             -}
scrollAmount st = max 1 (snd (clientWindowHeights actSize st))
               -- extra will be equal to main or 1 smaller
  where
    actSize = imageHeight (statusLineImage st)


-- | Number of lines to allocate for the focused window and the
-- main window. This doesn't include the textbox, activity bar,
-- or status line.
clientWindowHeights ::
  Int         {- ^ status bar height         -} ->
  ClientState {- ^ client state              -} ->
  (Int,Int)   {- ^ main height, extra height -}
clientWindowHeights statusBar st = (mainH, splitH)
  where
    h        = max 0 (view clientHeight st - overhead)
    splitH   = h `quot` (1+extras)
    mainH    = h - splitH*extras

    extras   = length (clientExtraFocuses st)
    textbox  = 1
    overhead = textbox + statusBar + 2*extras
