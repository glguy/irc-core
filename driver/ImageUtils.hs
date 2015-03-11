{-# LANGUAGE TemplateHaskell #-}
module ImageUtils where

import Data.Array
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
  | w <= width = [img <|> char defAttr ' '] -- vty forgets to turn off formatting
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
       "00" -> Just (white, t2) -- white
       "01" -> Just (black, t2) -- black
       "02" -> Just (blue, t2) -- blue
       "03" -> Just (green, t2) -- green
       "04" -> Just (red, t2) -- red
       "05" -> Just (rgbColor' 127 0 0, t2) -- brown
       "06" -> Just (rgbColor' 156 0 156, t2) -- purple
       "07" -> Just (rgbColor' 252 127 0, t2) -- yellow
       "08" -> Just (yellow, t2) -- yellow
       "09" -> Just (brightGreen, t2) -- green
       "10" -> Just (cyan, t2) -- brightBlue
       "11" -> Just (brightCyan, t2) -- brightCyan
       "12" -> Just (brightBlue, t2) -- brightBlue
       "13" -> Just (rgbColor' 255 0 255, t2)  -- brightRed
       "14" -> Just (rgbColor' 127 127 127, t2)-- brightBlack
       "15" -> Just (rgbColor' 210 210 210, t2)-- brightWhite
       _    -> Nothing

rgbColor' :: Int -> Int -> Int -> Color
rgbColor' = rgbColor -- fix the type to Int

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

-- | Render a string and replace the control characters with
-- reversed video of the associated control key.
stringWithControls :: String -> Image
stringWithControls [] = emptyImage
stringWithControls xs =
  case break isControl xs of
    (a,[]) -> string defAttr a
    (a,b:bs) -> string defAttr a
            <|> char (withStyle defAttr reverseVideo)
                     (controls ! fromEnum b)
            <|> stringWithControls bs

  where
  controls = listArray (0,0x1f) ('@':['A'..'Z']++"[\\]^_")
