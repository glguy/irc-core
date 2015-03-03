module Views.BanList where

import ClientState
import Control.Lens
import Data.ByteString (ByteString)
import Graphics.Vty.Image

import Irc.Format
import Irc.Model

banListImage :: Char -> Identifier -> ClientState -> Image
banListImage mode chan st =
  case view (clientConnection . connChannelIx chan . chanMaskLists . at mode) st of
    Nothing -> string (withForeColor defAttr red) "Unknown list"
    Just [] -> string (withForeColor defAttr green) "Empty list"
    Just xs -> startFromBottom st
             $ vertCat
             $ map renderEntry
             $ take (view clientHeight st - 4)
             $ drop (view clientScrollPos st) xs

renderEntry :: IrcMaskEntry -> Image
renderEntry entry =
      utf8Bytestring' (withForeColor defAttr red) (view maskEntryMask entry)
  <|> string defAttr " - "
  <|> utf8Bytestring' (withForeColor defAttr green) (view maskEntryWho entry)
  <|> string defAttr " - "
  <|> string (withForeColor defAttr yellow) (show (view maskEntryStamp entry))

startFromBottom :: ClientState -> Image -> Image
startFromBottom st img = pad 0 top 0 0 img
  where
  top = max 0 (view clientHeight st - 4 - imageHeight img)
