{-|
Module      : Client.View.Windows
Description : View of the list of open windows
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module implements the rendering of the client window list.

-}
module Client.View.Windows
  ( windowsImages
  ) where

import           Client.Configuration
import           Client.Image.Palette
import           Client.State
import           Client.State.Focus
import           Client.State.Window
import           Control.Lens
import           Data.List
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Graphics.Vty.Image
import           Irc.Identifier

-- | Draw the image lines associated with the @/windows@ command.
windowsImages :: ClientState -> [Image]
windowsImages st
  = reverse
  $ createColumns
  $ zipWith (renderWindowColumns pal) names windows
  where
    cfg     = view clientConfig st
    windows = views clientWindows Map.toAscList st

    pal     = view configPalette cfg
    names   = views configWindowNames Text.unpack cfg ++ repeat '?'

renderWindowColumns :: Palette -> Char -> (Focus, Window) -> [Image]
renderWindowColumns pal name (focus, win) =
  [ char (view palWindowName pal) name
  , renderedFocus pal focus
  , renderedWindowInfo pal win
  ]

createColumns :: [[Image]] -> [Image]
createColumns xs = map makeRow xs
  where
    columnWidths = maximum . map imageWidth <$> transpose xs
    makeRow = horizCat
            . intersperse (char defAttr ' ')
            . zipWith resizeWidth columnWidths

renderedFocus :: Palette -> Focus -> Image
renderedFocus pal focus =
  case focus of
    Unfocused ->
      char (view palError pal) '*'
    NetworkFocus network ->
      text' (view palLabel pal) network
    ChannelFocus network channel ->
      text' (view palLabel pal) network <|>
      char defAttr ':' <|>
      text' (view palLabel pal) (idText channel)

renderedWindowInfo :: Palette -> Window -> Image
renderedWindowInfo pal win =
  string (view newMsgAttrLens pal) (views winUnread show win) <|>
  char defAttr '/' <|>
  string (view palActivity pal) (views winTotal show win)
  where
    newMsgAttrLens
      | view winMention win = palMention
      | otherwise           = palActivity
