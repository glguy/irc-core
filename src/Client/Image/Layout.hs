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
import Client.View
import Graphics.Vty.Image
import Graphics.Vty.Attributes (defAttr)

-- | Compute the combined image for all the visible message windows.
drawLayout ::
  ClientState        {- ^ client state                 -} ->
  Int                {- ^ rows available               -} ->
  (Int, Image)       {- ^ overscroll and final image   -}
drawLayout st rows =
  case view clientLayout st of
    TwoColumn | not (null extrafocus) -> drawLayoutTwo st rows focus subfocus extrafocus
    _                                 -> drawLayoutOne st rows focus subfocus extrafocus
  where
    focus = view clientFocus st
    subfocus = view clientSubfocus st
    extrafocus = clientExtraFocuses st

-- | Layout algorithm for all windows in a single column.
drawLayoutOne ::
  ClientState        {- ^ client state                 -} ->
  Int                {- ^ rows available               -} ->
  Focus ->
  Subfocus ->
  [Focus] {- ^ extra window names -} ->
  (Int, Image)       {- ^ overscroll and final image   -}
drawLayoutOne st rows focus subfocus extrafocus = (overscroll, output)
  where
    w      = view clientWidth st
    h:hs   = splitHeights rows (length extraLines)
    scroll = view clientScroll st
    (overscroll, main) = messagePane w h scroll mainLines

    output = vertCat $ reverse
           $ main
           : [ drawExtra st w h' scroll foc imgs
                 | (h', (foc, imgs)) <- zip hs extraLines]

    mainLines = viewLines focus subfocus w st
    extraLines = [ (focus', viewLines focus' FocusMessages w st)
                   | focus' <- extrafocus ]

-- | Layout algorithm for all windows in a single column.
drawLayoutTwo ::
  ClientState        {- ^ client state                 -} ->
  Int                {- ^ rows available               -} ->
  Focus ->
  Subfocus ->
  [Focus] {- ^ extra window names -} ->
  (Int, Image)       {- ^ overscroll and final image   -}
drawLayoutTwo st h focus subfocus extrafocus = (overscroll, output)
  where
    [wl,wr] = divisions (view clientWidth st - 1) 2
    hs      = divisions (h - length extraLines) (length extraLines)
    scroll = view clientScroll st
    (overscroll, main) = messagePane wl h scroll mainLines

    output = main <|> divider <|> extraImgs
    extraImgs = vertCat $ reverse
             [ drawExtra st wr h' scroll foc imgs
                 | (h', (foc, imgs)) <- zip hs extraLines]

    pal     = clientPalette st
    divider = charFill (view palWindowDivider pal) ' ' 1 h

    mainLines = viewLines focus subfocus wl st
    extraLines = [ (focus', viewLines focus' FocusMessages wr st)
                   | focus' <- extrafocus ]


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
    msgImg <-> minorStatusLineImage focus st
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
    assemble acc (x:xs) = assemble (lineWrap w Nothing x <-> acc) xs


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
    h = view clientHeight st - bottomSize
    ex = length (clientExtraFocuses st)
    bottomSize = 1 -- textbox
               + imageHeight (statusLineImage st)
