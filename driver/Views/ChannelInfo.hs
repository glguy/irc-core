module Views.ChannelInfo where

import ClientState
import Control.Lens
import Data.ByteString (ByteString)
import Data.Monoid
import Graphics.Vty.Image
import ImageUtils
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Text as Text

import Irc.Format
import Irc.Model

channelInfoImage :: Identifier -> ClientState -> Image
channelInfoImage chan st =
  case view (clientConnection . connChannelAt chan) st of
    Nothing -> string (withForeColor defAttr red) "Unknown channel"
    Just channel -> vertCat
                  $ take (view clientHeight st - 4)
                  $ concatMap (lineWrap width)
                  $ topicLines
                 ++ creationLines
                 ++ modeLines
                 ++ urlLines
                 ++ usersLines
      where
      width = view clientWidth st

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
                          string defAttr (show modes)
                        ]

      urlLines =
        case view chanUrl channel of
          Nothing -> [string (withForeColor defAttr red) "Unknown URL"]
          Just url -> [ string (withForeColor defAttr green) "URL: " <|>
                        cleanText (asUtf8 url)
                      ]

      usersLines =
        [ string (withForeColor defAttr green) "Users: " <|>
          text' defAttr (Text.unwords (map (asUtf8 . idBytes) (Map.keys (view chanUsers channel))))
        ]
