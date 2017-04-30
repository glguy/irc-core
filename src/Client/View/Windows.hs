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

import           Client.Image.Palette
import           Client.State
import           Client.State.Focus
import           Client.State.Window
import           Client.State.Network
import           Control.Lens
import           Data.List
import qualified Data.Map as Map
import           Graphics.Vty.Attributes
import           Graphics.Vty.Image
import           Irc.Identifier

-- | Draw the image lines associated with the @/windows@ command.
windowsImages :: WindowsFilter -> ClientState -> [Image]
windowsImages filt st = reverse (createColumns windows)
  where
    windows = [ renderWindowColumns pal n k v
              | (n,(k,v)) <- zip names
                           $ views clientWindows Map.toAscList st
              , windowMatcher filt st k
              ]

    pal     = clientPalette st
    names   = clientWindowNames st ++ repeat '?'


------------------------------------------------------------------------

windowMatcher :: WindowsFilter -> ClientState -> Focus -> Bool

windowMatcher AllWindows _ _ = True

windowMatcher NetworkWindows _ NetworkFocus{} = True

windowMatcher ChannelWindows st (ChannelFocus net chan) =
  case preview (clientConnection net) st of
    Just cs -> isChannelIdentifier cs chan
    Nothing -> True

windowMatcher UserWindows st (ChannelFocus net chan) =
  case preview (clientConnection net) st of
    Just cs -> not (isChannelIdentifier cs chan)
    Nothing -> True

windowMatcher _ _ _ = False

------------------------------------------------------------------------


renderWindowColumns :: Palette -> Char -> Focus -> Window -> [Image]
renderWindowColumns pal name focus win =
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
    newMsgAttrLens =
      case view winMention win of
        WLImportant -> palMention
        _           -> palActivity
