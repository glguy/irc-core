module Views.BanList where

import ClientState
import Control.Lens
import Data.ByteString (ByteString)
import Graphics.Vty.Image
import Irc.Model

banListImage :: Char -> ByteString -> ClientState -> Image
banListImage mode chan st =
  case view (clientConnection . connChannelIx chan . chanMaskLists . at mode) st of
    Nothing -> string (withForeColor defAttr red) "Unknown list"
    Just [] -> string (withForeColor defAttr green) "Empty list"
    Just xs -> vertCat (map renderEntry (take (view clientHeight st - 4) xs))

renderEntry :: IrcMaskEntry -> Image
renderEntry entry =
      utf8Bytestring' (withForeColor defAttr red) (view maskEntryMask entry)
  <|> string defAttr " - "
  <|> utf8Bytestring' (withForeColor defAttr green) (view maskEntryWho entry)
  <|> string defAttr " - "
  <|> string (withForeColor defAttr yellow) (show (view maskEntryStamp entry))
                 -- TODO: Interpret timestamp
