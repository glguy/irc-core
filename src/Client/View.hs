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

import Client.Image.PackedImage (Image')
import Client.State
import Client.State.Focus
import Client.View.Cert (certViewLines)
import Client.View.ChannelList (channelListLines)
import Client.View.ChannelInfo (channelInfoImages)
import Client.View.Digraphs (digraphLines)
import Client.View.Help (helpImageLines)
import Client.View.IgnoreList (ignoreListLines)
import Client.View.KeyMap (keyMapLines)
import Client.View.MaskList (maskListImages)
import Client.View.Mentions (mentionsViewLines)
import Client.View.Messages (chatMessageImages)
import Client.View.Palette (paletteViewLines)
import Client.View.RtsStats (rtsStatsLines)
import Client.View.UrlSelection (urlSelectionView)
import Client.View.UserList (userInfoImages, userListImages)
import Client.View.Windows (windowsImages)
import Control.Lens (view)

viewLines :: Focus -> Subfocus -> Int -> ClientState -> [Image']
viewLines focus subfocus w !st =
  case (focus, subfocus) of
    _ | Just ("url",arg) <- clientActiveCommand st ->
      urlSelectionView w focus arg st
    (ChannelFocus network channel, FocusInfo) ->
      channelInfoImages network channel st
    (ChannelFocus network channel, FocusUsers)
      | view clientDetailView st -> userInfoImages network channel st
      | otherwise                -> userListImages network channel w st
    (ChannelFocus network channel, FocusMasks mode) ->
      maskListImages mode network channel w st
    (_, FocusWindows filt) -> windowsImages filt st
    (_, FocusMentions)     -> mentionsViewLines w st
    (_, FocusPalette)      -> paletteViewLines pal
    (_, FocusDigraphs)     -> digraphLines w st
    (_, FocusKeyMap)       -> keyMapLines st
    (_, FocusHelp mb)      -> helpImageLines st mb pal
    (_, FocusRtsStats)     -> rtsStatsLines (view clientRtsStats st) pal
    (_, FocusIgnoreList)   -> ignoreListLines (view clientIgnores st) pal
    (_, FocusCert)         -> certViewLines st
    (ChannelFocus network _, FocusChanList min' max') ->
      channelListLines network w st (min', max')
    (NetworkFocus network  , FocusChanList min' max') ->
      channelListLines network w st (min', max')
    _ -> chatMessageImages focus w st
  where
    pal = clientPalette st
