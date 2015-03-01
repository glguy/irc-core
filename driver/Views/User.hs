{-# LANGUAGE OverloadedStrings #-}
module Views.User (queryImage) where

import ClientState
import Control.Lens
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Graphics.Vty.Image
import ImageUtils

import Irc.Model
import Irc.Format

queryImage :: ClientState -> ByteString -> Image
queryImage st user =
  case preview (clientConnection . connUserIx user . usrMessages) st of
    Nothing -> string (withForeColor defAttr red) "Unknown user"
    Just msgs -> vertCat
               $ reverse
               $ take (view clientHeight st - 4)
               $ concatMap ( reverse
                           . lineWrap (view clientWidth st)
                           . renderOne
                           )
               $ toList msgs

renderOne :: IrcMessage -> Image
renderOne msg
  = utf8Bytestring' (withForeColor defAttr yellow) who <|>
    string (withForeColor defAttr blue) (": ") <|>
    cleanText txt
  where
  who = views mesgSender userNick msg
  txt = case view mesgType msg of
          PrivMsgType txt -> txt
          ActionMsgType txt -> txt
          NoticeMsgType txt -> txt
          _ -> "???"
