module Views.BanList where

import ClientState
import Control.Lens
import Data.ByteString (ByteString)
import Graphics.Vty.Image
import Irc.Model

banListImage :: ByteString -> ClientState -> Image
banListImage chan st =
  case preview (clientConnection . connChannelIx chan . chanBans . folded) st of
    Nothing -> string (withForeColor defAttr red) "Unknown ban list"
    Just [] -> string (withForeColor defAttr green) "Empty ban list"
    Just xs -> vertCat (map renderBan (take (view clientHeight st - 4) xs))

renderBan :: IrcBan -> Image
renderBan ban = utf8Bytestring' (withForeColor defAttr red) (view banBannee ban)
            <|> string defAttr " - "
            <|> utf8Bytestring' (withForeColor defAttr green) (view banBanner ban)
            <|> string defAttr " - "
            <|> string (withForeColor defAttr yellow) (show (view banStamp ban))
                 -- TODO: Interpret timestamp
