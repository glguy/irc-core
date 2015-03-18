module Views.BanList where

import ClientState
import Control.Lens
import Graphics.Vty.Image

import Irc.Format
import Irc.Model

banListImage :: Char -> Identifier -> ClientState -> [Image]
banListImage mode chan st =
  case view (connChannels . ix chan . chanMaskLists . at mode) conn of
    Nothing -> [string (withForeColor defAttr red) "Unknown list"]
    Just [] -> [string (withForeColor defAttr green) "Empty list"]
    Just xs -> map renderEntry xs

  where
  conn = view (clientServer0 . ccConnection) st

renderEntry :: IrcMaskEntry -> Image
renderEntry entry =
      utf8Bytestring' (withForeColor defAttr red) (view maskEntryMask entry)
  <|> string defAttr " - "
  <|> utf8Bytestring' (withForeColor defAttr green) (view maskEntryWho entry)
  <|> string defAttr " - "
  <|> string (withForeColor defAttr yellow) (show (view maskEntryStamp entry))
