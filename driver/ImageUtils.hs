{-# LANGUAGE TemplateHaskell #-}
module ImageUtils where

import Control.Lens
import Control.Monad (guard)
import Data.Array
import Data.ByteString (ByteString)
import Data.Char (isControl, isAlphaNum)
import Data.Set (Set)
import Data.Text (Text)
import Graphics.Vty.Image
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Set as Set
import qualified Data.Text as Text

import Irc.Format

data Formatting = Formatting
  { _fmtFore :: Maybe Color
  , _fmtBack :: Maybe Color
  , _fmtBold, _fmtItalic, _fmtUnderline, _fmtReverse
  , _fmtExplicit :: !Bool
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

  normalFormatting = defaultFormatting { _fmtExplicit = view fmtExplicit fmt }

  explicit img
    | view fmtExplicit fmt = img
    | otherwise = emptyImage

  rest = case Text.uncons b of
    Nothing -> emptyImage
    Just (b', xs) ->
      explicit (explicitControlImage b') <|>
      case b' of
        '\^B' -> ircFormattedText' (over fmtBold      not fmt) xs
        '\^O' -> ircFormattedText' normalFormatting            xs
        '\^V' -> ircFormattedText' (over fmtReverse   not fmt) xs
        '\^]' -> ircFormattedText' (over fmtItalic    not fmt) xs
        '\^_' -> ircFormattedText' (over fmtUnderline not fmt) xs
        '\^C'
          | Just (fore,xs1) <- colorNumber xs ->
             case Text.uncons xs1 of
               Just (',',xs2)
                  | Just (back,xs3) <- colorNumber xs2 ->
                      explicit (colorNumbers xs3) <|>
                      ircFormattedText'
                        (set fmtFore (Just fore)
                        (set fmtBack (Just back) fmt)) xs3

               _ -> explicit (colorNumbers xs1) <|>
                    ircFormattedText'
                      (set fmtFore (Just fore)
                      (set fmtBack Nothing fmt)) xs1

          | otherwise -> ircFormattedText' (set fmtFore Nothing (set fmtBack Nothing fmt)) xs
          where
            colorNumbers t = text' defAttr (Text.take (Text.length xs - Text.length t) xs)

        _ -> ircFormattedText' fmt xs

colorNumber :: Text -> Maybe (Color, Text)
colorNumber t =
  do (c1,c2,t1) <- splitNumber t
     case (c1,c2) of
       ('0','0') -> Just (white                , t1) -- white
       ('0','1') -> Just (black                , t1) -- black
       ('0','2') -> Just (blue                 , t1) -- blue
       ('0','3') -> Just (green                , t1) -- green
       ('0','4') -> Just (red                  , t1) -- red
       ('0','5') -> Just (rgbColor' 127 0 0    , t1) -- brown
       ('0','6') -> Just (rgbColor' 156 0 156  , t1) -- purple
       ('0','7') -> Just (rgbColor' 252 127 0  , t1) -- yellow
       ('0','8') -> Just (yellow               , t1) -- yellow
       ('0','9') -> Just (brightGreen          , t1) -- green
       ('1','0') -> Just (cyan                 , t1) -- brightBlue
       ('1','1') -> Just (brightCyan           , t1) -- brightCyan
       ('1','2') -> Just (brightBlue           , t1) -- brightBlue
       ('1','3') -> Just (rgbColor' 255 0 255  , t1) -- brightRed
       ('1','4') -> Just (rgbColor' 127 127 127, t1) -- brightBlack
       ('1','5') -> Just (rgbColor' 210 210 210, t1) -- brightWhite
       _         -> Nothing

-- Take up to two digits off the front of a text. If there is only
-- a single digit pretend like the first digit was a 0
splitNumber :: Text -> Maybe (Char,Char,Text)
splitNumber t =
  do let isNumber x = '0' <= x && x <= '9'
     (c1,t1) <- Text.uncons t
     guard (isNumber c1)
     case Text.uncons t1 of
       Just (c2,t2) | isNumber c2 -> Just (c1,c2,t2)
       _                          -> Just ('0',c1,t1)

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
  , _fmtExplicit  = False
  }

explicitFormatting :: Formatting
explicitFormatting = defaultFormatting { _fmtExplicit = True }

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
stringWithControls :: Attr -> String -> Image
stringWithControls _ [] = emptyImage
stringWithControls attr xs =
  case break isControl xs of
    (a,[]) -> string attr a
    (a,b:bs) -> string attr a
            <|> explicitControlImage b
            <|> stringWithControls attr bs

explicitControlImage :: Char -> Image
explicitControlImage x =
  char (withStyle defAttr reverseVideo)
       (controlNames ! fromEnum x)

controlNames :: Array Int Char
controlNames = listArray (0,0x1f) ('@':['A'..'Z']++"[\\]^_")

nameHighlighter ::
  ByteString -> Set Identifier -> Identifier -> [Color] -> Image
nameHighlighter msg users me colors = aux 0 0
  where
  lowmsg = ircFoldCase msg
  n = B8.length lowmsg
  ncolors = length colors

  aux lo hi
    | hi == n = utf8Bytestring' defAttr (B8.drop lo msg)
    | otherwise =
        case nameLookup identFromHi users of
          Nothing -> aux lo (advance hi)
          Just hit -> utf8Bytestring' defAttr
                        (B8.take (hi-lo) (B8.drop lo msg))
                      <|> utf8Bytestring' (withForeColor defAttr color) matchRegion
                      <|> aux hi' hi'
            where
            -- use the original match region to preserve original case
            matchRegion = B8.take (B8.length (idBytes hit)) (B8.drop hi msg)
            hi' = hi + B8.length (idDenote hit)
            color | me == hit = red
                  | otherwise = colors
                             !! mod (nickHash (idDenote hit)) ncolors

    where
    identFromHi = mkId (B8.drop hi lowmsg)

    advance curHi
      | curHi + 1 == n = curHi + 1
      | isAlphaNum (B8.index lowmsg curHi)
      , isAlphaNum (B8.index lowmsg (curHi+1)) = advance (curHi+1)
      | otherwise = curHi+1


nameLookup :: Identifier -> Set Identifier -> Maybe Identifier
nameLookup haystack s =
  case Set.lookupLE haystack s of
    Just x | idDenote x `B8.isPrefixOf` idDenote haystack
           , boundaryCheck (idDenote x) -> Just x
    _               -> Nothing
  where
  boundaryCheck needle =
    B8.length needle == B8.length (idDenote haystack) ||
    not (isAlphaNum (B8.index (idDenote haystack) (B8.length needle)))

nickHash :: ByteString -> Int
nickHash n =
  let h1 = B.foldl' (\acc b -> fromIntegral b + 33 * acc) 0 n
  in h1 + (h1 `quot` 32)


