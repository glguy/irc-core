{-# Language OverloadedStrings #-}
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

import           Client.Image.Focus (focusLabel, FocusLabelType(FocusLabelShort))
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.State
import           Client.State.Focus
import           Client.State.Window
import           Client.State.Network
import           Control.Lens
import           Data.List
import           Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import           Graphics.Vty.Attributes

-- | Draw the image lines associated with the @/windows@ command.
windowsImages :: WindowsFilter -> ClientState -> [Image']
windowsImages filt st
  = reverse
  $ createColumns
  $ [ renderWindowColumns st (char (view palError pal) 'h')    k v | (k,v) <- hiddenWindows ] ++
    [ renderWindowColumns st (char (view palWindowName pal) (name v)) k v | (k,v) <- windows ]
  where
    pal = clientPalette st
    name = fromMaybe ' ' . view winName

    (hiddenWindows, windows)
      = partition (view (_2 . winHidden))
      $ filter (windowMatcher filt st . fst)
      $ Map.toAscList
      $ view clientWindows st

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

renderWindowColumns :: ClientState -> Image' -> Focus -> Window -> [Image']
renderWindowColumns st name focus win =
  [ name
  , focusLabel FocusLabelShort st focus
  , renderedWindowInfo (clientPalette st) win
  ]

createColumns :: [[Image']] -> [Image']
createColumns xs = map makeRow xs
  where
    columnWidths = maximum . map imageWidth <$> transpose xs
    makeRow = mconcat
            . intersperse (char defAttr ' ')
            . zipWith resizeImage columnWidths

renderedWindowInfo :: Palette -> Window -> Image'
renderedWindowInfo pal win =
  string (view newMsgAttrLens pal) (views winUnread show win) <> "/" <>
  string (view palActivity    pal) (views winTotal  show win) <>
  case view winActivityFilter win of
    AFLoud -> mempty
    other -> string (view palMeta pal) (' ':show other)
  where
    newMsgAttrLens =
      case view winMention win of
        WLImportant -> palMention
        _           -> palActivity
