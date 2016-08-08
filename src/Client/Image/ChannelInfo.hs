{-# Language OverloadedStrings #-}
{-# Language BangPatterns #-}

{-|
Module      : Client.Image.ChannelInfo
Description : Channel information renderer
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module implements a renderer for the window that shows
channel metadata.

-}
module Client.Image.ChannelInfo
  ( channelInfoImages
  ) where

import           Client.ChannelState
import           Client.Configuration
import           Client.ConnectionState
import           Client.Image.Palette
import           Client.Image.Message
import           Client.MircFormatting
import           Client.State
import           Control.Lens
import           Data.Time
import           Graphics.Vty.Image
import           Irc.Identifier

-- | Render the lines used in a channel mask list
channelInfoImages :: NetworkName -> Identifier -> ClientState -> [Image]
channelInfoImages network channelId st

  | Just cs      <- preview (clientConnection network) st
  , Just channel <- preview (csChannels . ix channelId) cs
  = channelInfoImages' pal channel cs

  | otherwise = [text' (view palError pal) "No channel information"]
  where
    pal = view (clientConfig . configPalette) st

channelInfoImages' :: Palette -> ChannelState -> ConnectionState -> [Image]
channelInfoImages' pal !channel !cs
    = topicLine
    : provenanceLines
   ++ creationLines
   ++ urlLines

  where
    label = text' (view palLabel pal)

    topicLine = label "Topic: " <|> parseIrcText (view chanTopic channel)

    myNick = view csNick cs

    utcTimeImage = string defAttr . formatTime defaultTimeLocale "%F %T"

    provenanceLines =
        case view chanTopicProvenance channel of
          Nothing -> []
          Just !prov ->
            [ label "Topic set by: " <|>
                coloredUserInfo pal DetailedRender [myNick] (view topicAuthor prov)
            , label "Topic set on: " <|> utcTimeImage (view topicTime prov)
            ]

    creationLines =
        case view chanCreation channel of
          Nothing   -> []
          Just time -> [label "Created on: " <|> utcTimeImage time]

    urlLines =
        case view chanUrl channel of
          Nothing -> []
          Just url -> [ label "Channel URL: " <|> parseIrcText url ]

