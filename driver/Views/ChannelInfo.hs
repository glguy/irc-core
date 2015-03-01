module Views.ChannelInfo where

import ClientState
import Control.Lens
import Data.ByteString (ByteString)
import Data.Monoid
import Graphics.Vty.Image
import ImageUtils
import Irc.Model
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Text as Text

channelInfoImage :: ByteString -> ClientState -> Image
channelInfoImage chan st =
  case view (clientConnection . connChannelAt chan) st of
    Nothing -> string (withForeColor defAttr red) "Unknown channel"
    Just channel -> vertCat
                  $ take (view clientHeight st - 4)
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
          Just (topic, user, time) ->
            composeLine width (string (withForeColor defAttr green) "Topic: ") topic <>
            composeLine width (string (withForeColor defAttr green) "Set by: ") (asUtf8 user) <>
            composeLine width (string (withForeColor defAttr green) "Set on: ") (Text.pack (show time))

      creationLines =
        case view chanCreation channel of
          Nothing -> [string (withForeColor defAttr red) "Unknown creation time"]
          Just time -> composeLine width
                          (string (withForeColor defAttr green) "Created on: ") (Text.pack (show time))

      modeLines =
        case view chanModes channel of
          Nothing -> [string (withForeColor defAttr red) "Unknown mode"]
          Just modes -> composeLine width
                          (string (withForeColor defAttr green) "Mode: ")
                          (Text.pack (show modes))

      urlLines =
        case view chanUrl channel of
          Nothing -> [string (withForeColor defAttr red) "Unknown URL"]
          Just url -> composeLine width
                          (string (withForeColor defAttr green) "URL: ")
                          (asUtf8 url)

      usersLines =
        composeLine width
                    (string (withForeColor defAttr green) "Users: ")
                    (Text.unwords (map (asUtf8 . CI.original) (Map.keys (view chanUsers channel))))
