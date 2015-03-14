module Views.ChannelInfo where

import ClientState
import Control.Lens
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Monoid
import Graphics.Vty.Image
import ImageUtils
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map

import Irc.Format
import Irc.Model

channelInfoImage :: Identifier -> ClientState -> [Image]
channelInfoImage chan st =
  case view (clientConnection . connChannels . at chan) st of
    Nothing -> [string (withForeColor defAttr red) "Unknown channel"]
    Just channel -> topicLines
                 ++ creationLines
                 ++ modeLines
                 ++ urlLines
                 ++ usersLines
      where
      topicLines =
        case view chanTopic channel of
          Nothing -> [string (withForeColor defAttr red) "Unknown topic"]
          Just Nothing ->
            [ string (withForeColor defAttr green) "Empty Topic "
            ]
          Just (Just (topic, user, time)) ->
            [ string (withForeColor defAttr green) "Topic: "  <|> cleanText topic
            , string (withForeColor defAttr green) "Set by: " <|> cleanText (asUtf8 user)
            , string (withForeColor defAttr green) "Set on: " <|> string defAttr (show time)
            ]

      creationLines =
        case view chanCreation channel of
          Nothing -> [string (withForeColor defAttr red) "Unknown creation time"]
          Just time -> [ string (withForeColor defAttr green) "Created on: " <|>
                         string defAttr (show time)
                       ]

      modeLines =
        case view chanModes channel of
          Nothing -> [string (withForeColor defAttr red) "Unknown mode"]
          Just modes -> [ string (withForeColor defAttr green) "Mode: " <|>
                          utf8Bytestring' defAttr (renderModes modes)
                        ]

      urlLines =
        case view chanUrl channel of
          Nothing -> [string (withForeColor defAttr red) "Unknown URL"]
          Just url -> [ string (withForeColor defAttr green) "URL: " <|>
                        cleanText (asUtf8 url)
                      ]

      prefixes = view (clientConnection . connChanModeTypes . modesPrefixModes) st
      modePrefix modes =
        string (withForeColor defAttr blue)
        [ prefix | (mode,prefix) <- prefixes, mode `elem` modes ]

      usersLines
        = return
        $ horizCat
        $ string (withForeColor defAttr green) "Users:"
        : [ char defAttr ' ' <|>
            modePrefix modes <|>
            identImg defAttr nick
          | (nick,modes) <- Map.toList (view chanUsers channel)
          ]

renderModes :: Map Char ByteString -> ByteString
renderModes modes = B8.pack ('+':modeLetters)
                 <> B8.concat (map (B8.cons ' ')
                                   (filter (not . B8.null)
                                           modeArgs))
  where
  (modeLetters,modeArgs) = unzip (Map.toList modes)
