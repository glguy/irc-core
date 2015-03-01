module Views.Server where

import ClientState
import Control.Lens
import Graphics.Vty.Image
import Irc.Model
import ImageUtils
import qualified Data.Text as Text

serverInfoImage :: ClientState -> Image
serverInfoImage st = vertCat
                   $ take (height - 4)
                   $ drop (view clientScrollPos st)
                   $ concatMap (lineWrap width)
                   $ concat
                       [ myInfoLine
                       , channelsList
                       , motdLines
                       ]

  where
  width = view clientWidth st
  height = view clientHeight st

  myInfoLine =
    case view (clientConnection.connMyInfo) st of
      Nothing -> [string (withForeColor defAttr red)
                        "Unknown server host/version"]
      Just (host,version) ->
        [ string (withForeColor defAttr green) "Server: " <|>
          utf8Bytestring' defAttr host
        , string (withForeColor defAttr green) "Version: " <|>
          utf8Bytestring' defAttr version
        ]

  channels = views clientConnection activeChannelNames st
  channelsList
    | null channels = [string (withForeColor defAttr red)
                              "No active channels"]
    | otherwise =
          [ string (withForeColor defAttr green) "Channels: " <|>
            text' defAttr (Text.unwords (map asUtf8 channels))
          ]

  motdLines =
    case view (clientConnection . connMotd) st of
      Nothing -> [string (withForeColor defAttr red)
                             "No MOTD"]
      Just motd ->
        string (withForeColor defAttr green) "Message of the day:"
        : map (cleanText . asUtf8) motd
