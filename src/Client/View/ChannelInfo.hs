{-# Language OverloadedStrings #-}
{-# Language BangPatterns #-}

{-|
Module      : Client.View.ChannelInfo
Description : Channel information renderer
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module implements a renderer for the window that shows
channel metadata.

-}
module Client.View.ChannelInfo
  ( channelInfoImages
  ) where

import           Client.Image.Message
import           Client.Image.MircFormatting
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.State
import           Client.State.Channel
import           Client.State.Network
import           Control.Lens
import           Data.HashSet (HashSet)
import           Data.Text (Text)
import           Data.Time
import           Graphics.Vty.Attributes
import           Irc.Identifier

-- | Render the lines used in a channel mask list
channelInfoImages ::
  Text        {- ^ network -} ->
  Identifier  {- ^ channel -} ->
  ClientState -> [Image']
channelInfoImages network channelId st

  | Just cs      <- preview (clientConnection network) st
  , Just channel <- preview (csChannels . ix channelId) cs
  = channelInfoImages' pal (clientHighlights cs st) channel

  | otherwise = [text' (view palError pal) "No channel information"]
  where
    pal = clientPalette st

channelInfoImages' :: Palette -> HashSet Identifier -> ChannelState -> [Image']
channelInfoImages' pal myNicks !channel
    = topicLine
    : provenanceLines
   ++ creationLines
   ++ urlLines

  where
    label = text' (view palLabel pal)

    topicLine = label "Topic: " <>
                parseIrcText (view chanTopic channel)


    utcTimeImage = string defAttr . formatTime defaultTimeLocale "%F %T"

    provenanceLines =
        case view chanTopicProvenance channel of
          Nothing -> []
          Just !prov ->
            [ label "Topic set by: " <>
                coloredUserInfo pal DetailedRender myNicks (view topicAuthor prov)
            , label "Topic set on: " <> utcTimeImage (view topicTime prov)
            ]

    creationLines =
        case view chanCreation channel of
          Nothing   -> []
          Just time -> [label "Created on: " <> utcTimeImage time]

    urlLines =
        case view chanUrl channel of
          Nothing -> []
          Just url -> [ label "Channel URL: " <> parseIrcText url ]

