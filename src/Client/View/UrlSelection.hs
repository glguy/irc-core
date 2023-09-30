{-# Language OverloadedStrings #-}
{-|
Module      : Client.View.UrlSelection
Description : URL selection module
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a list of the URLs found in the current message
window in order to assist in selecting one to open with @/url@

-}
module Client.View.UrlSelection
  ( urlSelectionView
  ) where

import           Client.Configuration
import           Client.Image.Message
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.Image.LineWrap
import           Client.State
import           Client.State.Focus
import           Control.Lens
import           Data.HashMap.Strict (HashMap)
import           Data.List (intersperse, foldl1')
import           Data.Text (Text)
import           Graphics.Vty.Attributes
import           Irc.Identifier
import           Text.Read (readMaybe)


-- | Generate the lines used for the view when typing @/url@
urlSelectionView ::
  Int         {- ^ render width        -} ->
  Focus       {- ^ window to search    -} ->
  String      {- ^ argument to command -} ->
  ClientState {- ^ client state        -} ->
  [Image']    {- ^ image lines         -}
urlSelectionView w focus arg st
  = concat
  $ zipWith (draw w hilites pal selected) [1..] (urlList st)
  where
    focused = focus == view clientFocus st

    selected
      | not focused             = 0
      | all (==' ') arg         = 1
      | Just i <- readMaybe arg = i
      | otherwise               = 0 -- won't match

    cfg     = view clientConfig st
    pal     = view configPalette cfg

    hilites = clientHighlightsFocus focus st

-- | Render one line of the url list
draw ::
  Int                       {- ^ rendered width            -} ->
  HashMap Identifier Highlight {- ^ highlights             -} ->
  Palette                   {- ^ palette                   -} ->
  Int                       {- ^ selected index            -} ->
  Int                       {- ^ url index                 -} ->
  (Text, [Identifier])      {- ^ sender and url text       -} ->
  [Image']                  {- ^ rendered lines            -}
draw w hilites pal selected i (url, who)
  = reverse
  $ lineWrapPrefix w
      (string defAttr (shows i "."))
      (text' attr (cleanText url) <> who')
  where
    who'
      | null who = mempty
      | otherwise = " (" <> imgIds <> ")"
    imgIds = foldl1' (<>) $ intersperse ", " $ map idImg who
    idImg id' = coloredIdentifier pal NormalIdentifier hilites id'
    attr | selected == i = withStyle defAttr reverseVideo
         | otherwise     = defAttr
