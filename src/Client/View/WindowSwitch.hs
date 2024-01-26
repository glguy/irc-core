{-# Language OverloadedStrings #-}
{-|
Module      : Client.View.WindowSwitch
Description : Line renderers for window switcher
Copyright   : (c) TheDaemoness, 2024
License     : ISC
Maintainer  : emertens@gmail.com

This module renders lines used in the preview for /c
-}
module Client.View.WindowSwitch ( windowSwitchImages ) where

import           Client.Image.Focus (windowLabel)
import           Client.Image.PackedImage
import           Client.State
import           Client.State.Focus
import           Client.State.Window (winMention)
import           Control.Lens
import qualified Data.Map.Strict as Map
import           Data.List
import           Data.List.Split
import           Graphics.Vty (defAttr)

-- | Render the lines used by the @/c@ command.
windowSwitchImages ::
  String      {- ^ filter       -} ->
  Int         {- ^ window width -} ->
  ClientState {- ^ client state -} ->
  [Image']
windowSwitchImages arg w st = reverse [mconcat (intersperse gap row) | row <- chunksOf columns paddedNames]
  where
    paddedNames = map (resizeImage maxWidth) nameImages
    nameImages = map (windowLabel st) windowList
    maxWidth   = maximum (map imageWidth nameImages)
    columns    = max 1 ((w+1) `quot` (maxWidth+2))
    gap = string defAttr "  "
    windowList = sortBy activity . filter filterByFocus . Map.toAscList $ view clientWindows st
      where
        filterByFocus (focus,_) = isPrefixOfFocus arg focus
        activity (_,wa) (_,wb) = compare (view winMention wa) (view winMention wb)
