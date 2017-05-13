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
import Client.Image.Utils (lineWrap)
import Client.Image.Palette
import Graphics.Vty.Image
import Graphics.Vty.Attributes (defAttr)

-- | Compute the combined image for all the visible message windows.
drawLayout ::
  ClientState        {- ^ client state                 -} ->
  Int                {- ^ rows available               -} ->
  [Image]            {- ^ main window lines            -} ->
  [(Focus, [Image])] {- ^ extra window names and lines -} ->
  (Int, Image)       {- ^ overscroll and final image   -}
drawLayout st rows mainLines extraLines =
  case view clientLayout st of
    TwoColumn | not (null extraLines) -> drawLayoutTwo st rows mainLines extraLines
    _                                 -> drawLayoutOne st rows mainLines extraLines

-- | Layout algorithm for all windows in a single column.
drawLayoutOne ::
  ClientState        {- ^ client state                 -} ->
  Int                {- ^ rows available               -} ->
  [Image]            {- ^ main window lines            -} ->
  [(Focus, [Image])] {- ^ extra window names and lines -} ->
  (Int, Image)       {- ^ overscroll and final image   -}
drawLayoutOne st rows mainLines extraLines = (overscroll, output)
  where
    w      = view clientWidth st
    h:hs   = splitHeights rows (length extraLines)
    scroll = view clientScroll st
    (overscroll, main) = messagePane w h scroll mainLines

    output = vertCat $ reverse
           $ main
           : [ drawExtra st h' scroll foc imgs
                 | (h', (foc, imgs)) <- zip hs extraLines]

-- | Layout algorithm for all windows in a single column.
drawLayoutTwo ::
  ClientState        {- ^ client state                 -} ->
  Int                {- ^ rows available               -} ->
  [Image]            {- ^ main window lines            -} ->
  [(Focus, [Image])] {- ^ extra window names and lines -} ->
  (Int, Image)       {- ^ overscroll and final image   -}
drawLayoutTwo st h mainLines extraLines = (overscroll, output)
  where
    [wl,wr] = divisions (view clientWidth st - 1) 2
    hs      = divisions (h - 2 * length extraLines) (length extraLines)
    scroll = view clientScroll st
    (overscroll, main) = messagePane wl h scroll mainLines

    output = main <|> divider <|> extraImgs
    extraImgs = vertCat $ reverse
             [ drawExtra st h' scroll foc imgs
                 | (h', (foc, imgs)) <- zip hs extraLines]

    pal     = clientPalette st
    divider = charFill (view palWindowDivider pal) ' ' 1 h


-- | Draw one of the extra windows from @/splits@
drawExtra ::
  ClientState {- ^ client state    -} ->
  Int         {- ^ draw height     -} ->
  Int         {- ^ scroll amount   -} ->
  Focus       {- ^ focus           -} ->
  [Image]     {- ^ image lines     -} ->
  Image       {- ^ rendered window -}
drawExtra st h scroll focus lineImages =
    msgImg <-> minorStatusLineImage focus st <-> charFill divider ' ' w 1
  where
    w           = view clientWidth st
    pal         = clientPalette st
    divider     = view palWindowDivider pal
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
    assemble acc (x:xs) = assemble (lineWrap w Nothing x <-> acc) xs


splitHeights ::
  Int   {- ^ screen rows to fill               -} ->
  Int   {- ^ number of extra windows           -} ->
  [Int] {- ^ list of heights for each division -}
splitHeights h ex = divisions (h - 2 * ex) (1 + ex)


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
    h = view clientHeight st - bottomSize
    ex = length (clientExtraFocuses st)
    bottomSize = 1 -- textbox
               + imageHeight (statusLineImage st)
