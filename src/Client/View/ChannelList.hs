{-# Language OverloadedStrings #-}
{-|
Module      : Client.View.ChannelList
Description : Line renderer for searchable channel lists
Copyright   : (c) TheDaemoness, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module renders the lines used in the channel user list.
-}
module Client.View.ChannelList ( channelListLines ) where

import           Client.Image.LineWrap (lineWrapPrefix)
import           Client.Image.Message (IdentifierColorMode(NormalIdentifier), coloredIdentifier)
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.State
import           Client.State.Network
import           Control.Lens
import           Data.Text (Text)
import qualified Data.Text as Text
import           Graphics.Vty.Attributes (defAttr)
import           Irc.Identifier
import qualified Data.HashMap.Strict as HashMap
import Client.State.Focus (Subfocus(FocusChanList))

-- |
-- | Render the lines used by the @/list@ command in normal mode.
channelListLines ::
  Text        {- ^ network           -} ->
  Int         {- ^ window width      -} ->
  ClientState {- ^ client state      -} ->
  (Maybe Int, Maybe Int) {- ^ bounds -} ->
  [Image']
channelListLines network width st bounds =
  case preview (clientConnection network) st of
    Just cs -> channelListLines' cs width st bounds
    Nothing -> [text' (view palError pal) "No connection"]
  where
    pal = clientPalette st

channelListLines' ::
  NetworkState ->
  Int -> ClientState -> (Maybe Int, Maybe Int) -> [Image']
channelListLines' cs width st (min', max')
  | chanList^.clsDone = countImage : images
  | otherwise = countImagePending : images
  where
    chanList = cs^.csChannelList
    els = chanList^.clsElist
    pal = clientPalette st

    countImagePending = countImage <> text' (view palLabel pal) "..."
    countImage = text' (view palLabel pal) "Channels (visible/total): " <>
                 string defAttr (show (length entries')) <>
                 char (view palLabel pal) '/' <>
                 string defAttr (show (length entries)) <>
                 queryPart

    queryPart = mconcat $
      [text' (view palLabel pal) " More-than: " <> string defAttr (show lo) | FocusChanList (Just lo) _ <- [st^.clientSubfocus]] ++
      [text' (view palLabel pal) " Less-than: " <> string defAttr (show hi) | FocusChanList _ (Just hi) <- [st^.clientSubfocus]] ++
      [text' (view palLabel pal) " Elist: " <> text' defAttr txt | Just txt <- [els], not (Text.null txt)]

    entries = chanList^.clsItems
    entries' = clientFilterChannels st min' max' entries

    images = concatMap listItemImage entries'

    listItemImage :: (Identifier, Int, Text) -> [Image']
    listItemImage (chan, users, topic)
      | Text.null topic = [baseImage]
      | otherwise = reverse $ lineWrapPrefix width (baseImage <> label " topic:") (text' defAttr topic)
      where
        chanImage = coloredIdentifier pal NormalIdentifier HashMap.empty chan
        baseImage = chanImage <> label " users: " <> text' defAttr (Text.pack . show $ users)
        label = text' (view palLabel pal)
