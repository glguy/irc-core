{-# Language BangPatterns #-}
{-|
Module      : Client.View
Description : View selection module
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module selects the correct view based on the current state.

-}
module Client.View
  ( viewLines
  ) where

import           Client.Image.PackedImage
import           Client.State
import           Client.State.Focus
import           Client.View.ChannelInfo
import           Client.View.Digraphs
import           Client.View.Help
import           Client.View.KeyMap
import           Client.View.MaskList
import           Client.View.Mentions
import           Client.View.Messages
import           Client.View.Palette
import           Client.View.UrlSelection
import           Client.View.UserList
import           Client.View.Windows
import           Control.Lens

viewLines :: Focus -> Subfocus -> Int -> ClientState -> [Image']
viewLines focus subfocus w !st =
  case (focus, subfocus) of
    _ | Just ("url",arg) <- clientActiveCommand st ->
      urlSelectionView w focus arg st
    (ChannelFocus network channel, FocusInfo) ->
      channelInfoImages network channel st
    (ChannelFocus network channel, FocusUsers)
      | view clientDetailView st -> userInfoImages network channel st
      | otherwise                -> userListImages network channel st
    (ChannelFocus network channel, FocusMasks mode) ->
      maskListImages mode network channel w st
    (_, FocusWindows filt) -> windowsImages filt st
    (_, FocusMentions) -> mentionsViewLines w st
    (_, FocusPalette) -> paletteViewLines pal
    (_, FocusDigraphs) -> digraphLines w st
    (_, FocusKeyMap) -> keyMapLines st
    (_, FocusHelp mb) -> helpImageLines mb pal
    _ -> chatMessageImages focus w st
  where
    pal = clientPalette st
