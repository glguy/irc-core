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
import           Client.Message
import           Client.State
import           Client.State.Focus
import           Client.State.Network
import           Client.State.Window
import           Control.Lens
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Semigroup
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
  $ zipWith (draw w me pal padding selected) [1..] (toListOf urled st)
  where
    urled = clientWindows . ix focus
          . winMessages   . each
          . folding matches

    focused = focus == view clientFocus st

    selected
      | not focused             = 0
      | all (==' ') arg         = 1
      | Just i <- readMaybe arg = i
      | otherwise               = 0 -- won't match

    cfg     = view clientConfig st
    padding = view configNickPadding cfg
    pal     = view configPalette cfg

    me      = maybe HashSet.empty HashSet.singleton
            $ do net <- focusNetwork focus
                 preview (clientConnection net . csNick) st


matches :: WindowLine -> [(Maybe Identifier, Text)]
matches wl = [ (views wlSummary summaryActor wl, url) | url <- views wlText urlMatches wl ]

summaryActor :: IrcSummary -> Maybe Identifier
summaryActor s =
  case s of
    JoinSummary who   -> Just who
    QuitSummary who   -> Just who
    PartSummary who   -> Just who
    NickSummary who _ -> Just who
    ChatSummary who   -> Just who
    CtcpSummary who   -> Just who
    ReplySummary {}   -> Nothing
    NoSummary         -> Nothing


-- | Render one line of the url list
draw ::
  Int                       {- ^ rendered width            -} ->
  HashSet Identifier        {- ^ my nick                   -} ->
  Palette                   {- ^ palette                   -} ->
  PaddingMode               {- ^ nick render padding       -} ->
  Int                       {- ^ selected index            -} ->
  Int                       {- ^ url index                 -} ->
  (Maybe Identifier, Text)  {- ^ sender and url text       -} ->
  [Image']                  {- ^ rendered lines            -}
draw w me pal padding selected i (who,url)
  = reverse
  $ lineWrapPrefix w
      (string defAttr (shows i ". ") <>
       nickPad padding
         (foldMap (coloredIdentifier pal NormalIdentifier me) who) <> ": ")
      (text' attr (cleanText url))
  where
    attr | selected == i = withStyle defAttr reverseVideo
         | otherwise     = defAttr
