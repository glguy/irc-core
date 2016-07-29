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
import           Client.ConnectionState
import           Client.Image.Message
import           Client.MircFormatting
import           Client.State
import           Control.Lens
import qualified Data.HashMap.Strict as HashMap
import           Data.List
import           Data.Ord
import           Data.Time
import           Graphics.Vty.Image
import           Irc.Identifier

-- | Render the lines used in a channel mask list
channelInfoImages :: NetworkName -> Identifier -> ClientState -> [Image]
channelInfoImages network channelId st

  | Just cs      <- preview (clientConnection network) st
  , Just channel <- preview (csChannels . ix channelId) cs
  = channelInfoImages' channel cs

  | otherwise = [text' (withForeColor defAttr red) "No channel information"]

channelInfoImages' :: ChannelState -> ConnectionState -> [Image]
channelInfoImages' !channel !cs
    = topicLine
    : provenanceLines
   ++ creationLines
   ++ urlLines

  where
    label = text' (withForeColor defAttr green)

    topicLine = label "Topic: " <|> parseIrcText (view chanTopic channel)

    myNick = view csNick cs

    utcTimeImage = string defAttr . formatTime defaultTimeLocale "%F %T"

    provenanceLines =
        case view chanTopicProvenance channel of
          Nothing -> []
          Just !prov ->
            [ label "Topic set by: " <|> coloredUserInfo DetailedRender [myNick] (view topicAuthor prov)
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

