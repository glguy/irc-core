{-# LANGUAGE TemplateHaskell #-}
module ImageUtils where

import Data.Text (Text)
import Data.Char (isControl)
import qualified Data.Text as Text
import Graphics.Vty.Image
import Control.Lens

import Irc.Format

data Formatting = Formatting
  { _fmtFore :: Maybe Color
  , _fmtBack :: Maybe Color
  , _fmtBold, _fmtItalic, _fmtUnderline, _fmtReverse :: !Bool
  }

makeLenses ''Formatting

lineWrap :: Int -> Image -> [Image]
lineWrap width img
  | w <= width = [img]
  | otherwise = cropRight width img : lineWrap width (cropLeft (w-width) img)
  where
  w = imageWidth img

cleanText :: Text -> Image
cleanText = ircFormattedText

renderFullUsermask :: UserInfo -> Image
renderFullUsermask u
   = utf8Bytestring' (withForeColor defAttr yellow) (idBytes (userNick u))
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

ircFormattedText :: Text -> Image
ircFormattedText = ircFormattedText' defaultFormatting

ircFormattedText' :: Formatting -> Text -> Image
ircFormattedText' fmt t = text' (formattingAttr fmt) a <|> rest
  where
  (a,b) = Text.break isControl t

  rest = case Text.uncons b of
    Nothing -> emptyImage
    Just ('\x02',xs) -> ircFormattedText' (over fmtBold      not fmt) xs
    Just ('\x0F',xs) -> ircFormattedText' defaultFormatting           xs
    Just ('\x16',xs) -> ircFormattedText' (over fmtReverse   not fmt) xs
    Just ('\x1D',xs) -> ircFormattedText' (over fmtItalic    not fmt) xs
    Just ('\x1F',xs) -> ircFormattedText' (over fmtUnderline not fmt) xs
    Just ('\x03',xs)
      | Just (fore,xs1) <- colorNumber xs ->
         case Text.uncons xs1 of
           Just (',',xs2)
              | Just (back,xs3) <- colorNumber xs2 -> ircFormattedText'
                                                        (set fmtFore (Just fore)
                                                        (set fmtBack (Just back) fmt)) xs3
           _ -> ircFormattedText' (set fmtFore (Just fore)
                                  (set fmtBack Nothing fmt)) xs1
      | otherwise -> ircFormattedText' (set fmtFore Nothing (set fmtBack Nothing fmt)) xs

    Just (_,xs) -> ircFormattedText' fmt xs

colorNumber :: Text -> Maybe (Color, Text)
colorNumber t =
  do (f1,t1) <- Text.uncons t
     (f2,t2) <- Text.uncons t1
     case [f1,f2] of
       "00" -> Just (white, t2)
       "01" -> Just (black, t2)
       "02" -> Just (blue, t2)
       "03" -> Just (green, t2)
       "04" -> Just (red, t2)
       "05" -> Just (cyan, t2)
       "06" -> Just (magenta, t2)
       "07" -> Just (yellow, t2)
       "08" -> Just (yellow, t2)
       "09" -> Just (green, t2)
       "10" -> Just (brightBlue, t2)
       "11" -> Just (brightCyan, t2)
       "12" -> Just (brightBlue, t2)
       "13" -> Just (brightRed, t2)
       "14" -> Just (brightBlack, t2)
       "15" -> Just (brightWhite, t2)
       _    -> Nothing


defaultFormatting :: Formatting
defaultFormatting = Formatting
  { _fmtFore      = Nothing
  , _fmtBack      = Nothing
  , _fmtBold      = False
  , _fmtItalic    = False
  , _fmtUnderline = False
  , _fmtReverse   = False
  }

formattingAttr :: Formatting -> Attr
formattingAttr fmt
  = addForeColor
  $ addBackColor
  $ flag (view fmtBold      fmt) bold
  $ flag (view fmtUnderline fmt) underline
  $ flag (view fmtReverse   fmt) reverseVideo
  -- no italic support
  $ defAttr

  where
  addForeColor x =
    case view fmtFore fmt of
      Nothing -> x
      Just c  -> withForeColor x c

  addBackColor x =
    case view fmtBack fmt of
      Nothing -> x
      Just c  -> withBackColor x c

  flag True  s x = withStyle x s
  flag False _ x = x

identImg :: Attr -> Identifier -> Image
identImg attr = utf8Bytestring' attr . idBytes
