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
  ( viewMainLines
  , viewExtraLines
  ) where

import Client.Image.PackedImage (Image')
import Client.State
import Client.State.Focus
import Client.State.Help (hsImages)
import Client.View.Cert (certViewLines)
import Client.View.ChannelList (channelListLines)
import Client.View.ChannelInfo (channelInfoImages)
import Client.View.Digraphs (digraphLines)
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
import Client.View.WindowSwitch (windowSwitchImages)
import Control.Lens (view)

-- | Generate the lines for the main window (the one associated with text input)
viewMainLines :: Focus -> Subfocus -> Int -> ClientState -> [Image']
viewMainLines focus subfocus w !st
  | Just ("url",arg) <- clientActiveCommand st = urlSelectionView w (actualFocus subfocus focus) arg st
  | Just ("c"  ,arg) <- clientActiveCommand st = windowSwitchImages arg w st
  | otherwise                                  = viewExtraLines focus subfocus w st

-- | Generate the lines for an extra window. This is like 'viewMainLines' except
-- it doesn't react to the current state of the input bar.
viewExtraLines :: Focus -> Subfocus -> Int -> ClientState -> [Image']
viewExtraLines focus subfocus w !st =
  case subfocus of
    FocusInfo network channel ->
      channelInfoImages network channel st
    FocusUsers network channel
      | view clientDetailView st -> userInfoImages network channel st
      | otherwise                -> userListImages network channel w st
    FocusMasks network channel mode ->
      maskListImages mode network channel w st
    FocusWindows filt -> windowsImages filt st
    FocusMentions     -> mentionsViewLines w st
    FocusPalette      -> paletteViewLines pal
    FocusDigraphs     -> digraphLines w st
    FocusKeyMap       -> keyMapLines st
    FocusRtsStats     -> rtsStatsLines (view clientRtsStats st) pal
    FocusIgnoreList   -> ignoreListLines (view clientIgnores st) pal
    FocusCert         -> certViewLines st
    FocusChanList network min' max' ->
      channelListLines network w st (min', max')
    FocusWho network ->
      whoLines network w st
    FocusHelp         -> view (clientHelp . hsImages) st
    _ -> chatMessageImages focus w st
  where
    pal = clientPalette st
