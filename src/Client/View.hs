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

import           Client.Configuration
import           Client.State
import           Client.State.Focus
import           Client.View.ChannelInfo
import           Client.View.Help
import           Client.View.MaskList
import           Client.View.Mentions
import           Client.View.Messages
import           Client.View.Palette
import           Client.View.UserList
import           Client.View.Windows
import           Control.Lens
import           Graphics.Vty.Image

viewLines :: ClientState -> [Image]
viewLines !st =
  case (view clientFocus st, view clientSubfocus st) of
    (ChannelFocus network channel, FocusInfo) ->
      channelInfoImages network channel st
    (ChannelFocus network channel, FocusUsers)
      | view clientDetailView st -> userInfoImages network channel st
      | otherwise                -> userListImages network channel st
    (ChannelFocus network channel, FocusMasks mode) ->
      maskListImages mode network channel st
    (_, FocusWindows) -> windowsImages st
    (_, FocusMentions) -> mentionsViewLines st
    (_, FocusPalette) -> paletteViewLines pal
    (_, FocusHelp mb) -> helpImageLines mb pal

    _ -> chatMessageImages st
  where
    pal = view (clientConfig . configPalette) st
