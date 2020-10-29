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

import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.State
import           Client.State.Focus
import           Client.State.Window
import           Client.State.Network
import           Control.Lens
import           Data.List
import qualified Data.Map as Map
import           Graphics.Vty.Attributes
import           Irc.Identifier

-- | Draw the image lines associated with the @/windows@ command.
windowsImages :: WindowsFilter -> ClientState -> [Image']
windowsImages filt st
  = reverse
  $ createColumns
  $ [ renderWindowColumns pal (char (view palError pal) 'h')    k v |    (k,v)  <- hiddenWindows     ] ++
    [ renderWindowColumns pal (char (view palWindowName pal) n) k v | (n,(k,v)) <- zip names windows ]
  where
    pal     = clientPalette st
    names   = clientWindowNames st ++ repeat ' '

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


renderWindowColumns :: Palette -> Image' -> Focus -> Window -> [Image']
renderWindowColumns pal name focus win =
  [ name
  , renderedFocus pal focus
  , renderedWindowInfo pal win
  ]


createColumns :: [[Image']] -> [Image']
createColumns xs = map makeRow xs
  where
    columnWidths = maximum . map imageWidth <$> transpose xs
    makeRow = mconcat
            . intersperse (char defAttr ' ')
            . zipWith resizeImage columnWidths

renderedFocus :: Palette -> Focus -> Image'
renderedFocus pal focus =
  case focus of
    Unfocused ->
      char (view palError pal) '*'
    NetworkFocus network ->
      text' (view palLabel pal) network
    ChannelFocus network channel ->
      text' (view palLabel pal) network <>
      char defAttr ':' <>
      text' (view palLabel pal) (idText channel)

renderedWindowInfo :: Palette -> Window -> Image'
renderedWindowInfo pal win =
  string (view newMsgAttrLens pal) (views winUnread show win) <> "/" <>
  string (view palActivity    pal) (views winTotal  show win) <>
  (if view winSilent win then text' (view palMeta pal) " silent" else mempty)
  where
    newMsgAttrLens =
      case view winMention win of
        WLImportant -> palMention
        _           -> palActivity
