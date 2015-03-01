module ImageUtils where

import Data.Text (Text)
import Data.Char (isControl)
import qualified Data.Text as Text
import Graphics.Vty.Image
import Network.IRC.ByteString.Parser

composeLine :: Int -> Image -> Text -> [Image]
composeLine width header txt = firstLine : aux wrapTxt
  where
  firstLine = header <|> text' defAttr firstTxt
  txtClean = Text.filter (not . isControl) txt
  (firstTxt,wrapTxt) = Text.splitAt (width - imageWidth header) txtClean

  aux t
    | Text.null t = []
    | otherwise   = text' defAttr a : aux b
    where
    (a,b) = Text.splitAt width t

renderFullUsermask :: Either UserInfo ServerName -> Image
renderFullUsermask ui =
  case ui of
    Right s -> utf8Bytestring' defAttr s
    Left u  -> utf8Bytestring' (withForeColor defAttr yellow) (userNick u)
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


