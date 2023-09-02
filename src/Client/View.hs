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
import Client.View.Who (whoLines)
import Client.View.Windows (windowsImages)
import Control.Lens (view)

viewLines :: Focus -> Subfocus -> Int -> ClientState -> [Image']
viewLines focus subfocus w !st =
  case (network', channel', subfocus) of
    _ | Just ("url",arg) <- clientActiveCommand st ->
      urlSelectionView w focus arg st
    (Just network, Just channel, FocusInfo) ->
      channelInfoImages network channel st
    (Just network, Just channel, FocusUsers)
      | view clientDetailView st -> userInfoImages network channel st
      | otherwise                -> userListImages network channel w st
    (Just network, Just channel, FocusMasks mode) ->
      maskListImages mode network channel w st
    (_, _, FocusWindows filt) -> windowsImages filt st
    (_, _, FocusMentions)     -> mentionsViewLines w st
    (_, _, FocusPalette)      -> paletteViewLines pal
    (_, _, FocusDigraphs)     -> digraphLines w st
    (_, _, FocusKeyMap)       -> keyMapLines st
    (_, _, FocusHelp mb)      -> helpImageLines st mb pal
    (_, _, FocusRtsStats)     -> rtsStatsLines (view clientRtsStats st) pal
    (_, _, FocusIgnoreList)   -> ignoreListLines (view clientIgnores st) pal
    (_, _, FocusCert)         -> certViewLines st
    (Just network, _, FocusChanList min' max') ->
      channelListLines network w st (min', max')
    (Just network, _, FocusWho) ->
      whoLines network w st
    _ -> chatMessageImages focus w st
  where
    (network', channel') = case focus of
      Unfocused -> (Nothing, Nothing)
      NetworkFocus network -> (Just network, Nothing)
      ChannelFocus network channel -> (Just network, Just channel)
    pal = clientPalette st
