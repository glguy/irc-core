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
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.State
import           Client.State.Network
import           Control.Lens
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import           Graphics.Vty.Attributes (defAttr)
import           Irc.Identifier

-- | Render the lines used by the @/list@ command in normal mode.
channelListLines ::
  Text        {- ^ network              -} ->
  Int         {- ^ window width         -} ->
  ClientState {- ^ client state         -} ->
  [Image']
channelListLines network width st =
  case preview (clientConnection network) st of
    Just cs -> channelListLines' cs width st
    Nothing -> [text' (view palError pal) "No connection"]
  where
    pal = clientPalette st

channelListLines' :: NetworkState -> Int -> ClientState -> [Image']
channelListLines' cs width st
  | (chanList^.clsDone) = countImage : images
  | otherwise = countImagePending : images
  where
    chanList = cs^.csChannelList
    pal = clientPalette st

    countImagePending = countImage <> text' (view palLabel pal) "..."
    countImage = text' (view palLabel pal) "Channels (visible/total): " <>
                 string defAttr (show (length entries')) <>
                 char (view palLabel pal) '/' <>
                 string defAttr (show (length entries))

    entries = chanList^.clsItems
    entries' = clientFilter st filterOn entries
    filterOn (chan, _, topic) = LText.fromChunks [idText chan, " ", topic]

    images = concatMap listItemImage entries'

    listItemImage :: (Identifier, Int, Text) -> [Image']
    listItemImage (chan, users, topic)
      | Text.null topic = [baseImage]
      | otherwise = reverse $ lineWrapPrefix width (baseImage <> label " topic:") (text' defAttr topic)
      where
        baseImage = text' defAttr (idText chan) <> label " users: " <> text' defAttr (Text.pack . show $ users)
        label = text' (view palLabel pal)
