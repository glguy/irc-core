module ImageUtils where

import Data.Text (Text)
import Data.Char (isControl)
import qualified Data.Text as Text
import Graphics.Vty.Image

import Irc.Format

lineWrap :: Int -> Image -> [Image]
lineWrap width img
  | w <= width = [img]
  | otherwise = cropRight width img : lineWrap width (cropLeft (w-width) img)
  where
  w = imageWidth img

cleanText :: Text -> Image
cleanText = text' defAttr . Text.filter (not . isControl)

renderFullUsermask :: UserInfo -> Image
renderFullUsermask u
   = utf8Bytestring' (withForeColor defAttr yellow) (userNick u)
  <|> userpart
  <|> hostpart
      where
      userpart = case userName u of
                   Just x -> string defAttr "!"
                         <|> utf8Bytestring' (withForeColor defAttr green) x
                   Nothing -> emptyImage
      hostpart = case userHost u of
                   Just x -> string defAttr "@"
                         <|> utf8Bytestring' (withForeColor defAttr red) x
                   Nothing -> emptyImage


