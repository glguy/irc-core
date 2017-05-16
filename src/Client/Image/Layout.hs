{-|
Module      : Client.Image.Layout
Description : Layout code for the multi-window splits
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Client.Image.Layout (scrollAmount, drawLayout) where

import Control.Lens
import Client.State
import Client.State.Focus
import Client.Configuration (LayoutMode(..))
import Client.Image.StatusLine (statusLineImage, minorStatusLineImage)
import Client.Image.Textbox
import Client.Image.LineWrap (lineWrap)
import Client.Image.Palette
import Client.View
import Graphics.Vty.Image
import Graphics.Vty.Attributes (defAttr)

-- | Compute the combined image for all the visible message windows.
drawLayout ::
  ClientState            {- ^ client state                                     -} ->
  (Int, Int, Int, Image) {- ^ overscroll, cursor pos, next offset, final image -}
drawLayout st =
  case view clientLayout st of
    TwoColumn | not (null extrafocus) -> drawLayoutTwo st extrafocus
    _                                 -> drawLayoutOne st extrafocus
  where
    extrafocus = clientExtraFocuses st

-- | Layout algorithm for all windows in a single column.
drawLayoutOne ::
  ClientState            {- ^ client state                 -} ->
  [Focus]                {- ^ extra window names           -} ->
  (Int, Int, Int, Image) {- ^ overscroll and final image   -}
drawLayoutOne st extrafocus =
  (overscroll, pos, nextOffset, output)
  where
    w      = view clientWidth st
    h:hs   = splitHeights (rows - saveRows) (length extraLines)
    scroll = view clientScroll st

    (overscroll, pos, nextOffset, main) =
        drawMain w (saveRows + h) scroll st

    output = vertCat $ reverse
           $ main
           : [ drawExtra st w h' scroll foc imgs
                 | (h', (foc, imgs)) <- zip hs extraLines]

    rows = view clientHeight st

    -- don't count textbox or the main status line against the main window's height
    saveRows = 1 + imageHeight (statusLineImage w st)

    extraLines = [ (focus', viewLines focus' FocusMessages w st)
                   | focus' <- extrafocus ]

-- | Layout algorithm for all windows in a single column.
drawLayoutTwo ::
  ClientState            {- ^ client state                                -} ->
  [Focus]                {- ^ extra window names                          -} ->
  (Int, Int, Int, Image) {- ^ overscroll, cursor pos, offset, final image -}
drawLayoutTwo st extrafocus =
  (overscroll, pos, nextOffset, output)
  where
    [wl,wr] = divisions (view clientWidth st - 1) 2
    hs      = divisions (rows - length extraLines) (length extraLines)
    scroll = view clientScroll st

    output = main <|> divider <|> extraImgs

    extraImgs = vertCat $ reverse
             [ drawExtra st wr h' scroll foc imgs
                 | (h', (foc, imgs)) <- zip hs extraLines]

    (overscroll, pos, nextOffset, main) =
        drawMain wl rows scroll st

    pal     = clientPalette st
    divider = charFill (view palWindowDivider pal) ' ' 1 rows
    rows    = view clientHeight st

    extraLines = [ (focus', viewLines focus' FocusMessages wr st)
                   | focus' <- extrafocus ]

drawMain ::
  Int         {- ^ draw width      -} ->
  Int         {- ^ draw height     -} ->
  Int         {- ^ scroll amount   -} ->
  ClientState {- ^ client state    -} ->
  (Int,Int,Int,Image)
drawMain w h scroll st = (overscroll, pos, nextOffset, msgs <-> bottomImg)
  where
    focus = view clientFocus st
    subfocus = view clientSubfocus st

    msgLines = viewLines focus subfocus w st

    (overscroll, msgs) = messagePane w h' scroll msgLines

    h' = max 0 (h - imageHeight bottomImg)

    bottomImg = statusLineImage w st <-> tbImage
    (pos, nextOffset, tbImage) = textboxImage w st


-- | Draw one of the extra windows from @/splits@
drawExtra ::
  ClientState {- ^ client state    -} ->
  Int         {- ^ draw width      -} ->
  Int         {- ^ draw height     -} ->
  Int         {- ^ scroll amount   -} ->
  Focus       {- ^ focus           -} ->
  [Image]     {- ^ image lines     -} ->
  Image       {- ^ rendered window -}
drawExtra st w h scroll focus lineImages =
    msgImg <-> minorStatusLineImage focus w True st
  where
    (_, msgImg) = messagePane w h scroll lineImages


-- | Generate an image corresponding to the image lines of the given
-- focus and subfocus. Returns the number of lines overscrolled to
-- assist in clamping scroll to the lines available in the window.
messagePane ::
  Int          {- ^ client width                  -} ->
  Int          {- ^ available rows                -} ->
  Int          {- ^ current scroll                -} ->
  [Image]      {- ^ focused window                -} ->
  (Int, Image) {- ^ overscroll, rendered messages -}
messagePane w h scroll images = (overscroll, img)
  where
    vimg   = assemble emptyImage images
    vimg1  = cropBottom h vimg
    img    = charFill defAttr ' ' w (h - imageHeight vimg1)
             <-> vimg1

    overscroll = vh - imageHeight vimg
    vh         = h + scroll

    assemble acc _ | imageHeight acc >= vh = cropTop vh acc
    assemble acc [] = acc
    assemble acc (x:xs) = assemble (lineWrap w x <-> acc) xs


splitHeights ::
  Int   {- ^ screen rows to fill               -} ->
  Int   {- ^ number of extra windows           -} ->
  [Int] {- ^ list of heights for each division -}
splitHeights h ex = divisions (h - ex) (1 + ex)


-- | Constructs a list of numbers with the length of the divisor
-- and that sums to the dividend. Each element will be within
-- one of the quotient.
divisions ::
  Int {- ^ dividend -} ->
  Int {- ^ divisor  -} ->
  [Int]
divisions x y
  | y <= 0    = []
  | otherwise = replicate r (q+1) ++ replicate (y-r) q
  where
    (q,r) = quotRem (max 0 x) y



-- | Compute the number of lines in a page at the current window size
scrollAmount ::
  ClientState {- ^ client state  -} ->
  Int         {- ^ scroll amount -}
scrollAmount st =
  case view clientLayout st of
    TwoColumn -> h
    OneColumn -> head (splitHeights h ex) -- extra will be equal to main or 1 smaller
  where
    layout = view clientLayout st

    h = view clientHeight st - bottomSize
    ex = length (clientExtraFocuses st)

    bottomSize = 1 -- textbox
               + imageHeight (statusLineImage mainWidth st)

    mainWidth =
      case layout of
        TwoColumn -> head (divisions (view clientWidth st - 1) 2)
        OneColumn -> view clientWidth st
