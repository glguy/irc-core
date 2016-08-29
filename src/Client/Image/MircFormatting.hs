{-# Language TemplateHaskell #-}

{-|
Module      : Client.Image.MircFormatting
Description : Parser for mIRC's text formatting encoding
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module parses mIRC encoded text and generates VTY images.

-}
module Client.Image.MircFormatting
  ( parseIrcText
  , parseIrcTextExplicit
  , controlImage
  ) where

import           Control.Applicative ((<|>))
import           Control.Lens
import           Data.Attoparsec.Text as Parse
import           Data.Char
import           Data.Maybe
import           Data.Text (Text)
import           Graphics.Vty hiding ((<|>))
import qualified Graphics.Vty as Vty

data FormatState = FormatState
  { _fmtFore :: Maybe Color
  , _fmtBack :: Maybe Color
  , _fmtBold, _fmtItalic, _fmtUnderline, _fmtReverse :: !Bool
  }

makeLenses '' FormatState

formatAttr :: FormatState -> Attr
formatAttr fmt
  = doStyle (view fmtBold fmt) bold
  $ doStyle (view fmtUnderline fmt) underline
  $ doStyle (view fmtReverse fmt) reverseVideo
  $ doColor withForeColor (view fmtFore fmt)
  $ doColor withBackColor (view fmtBack fmt)
  $ defAttr

  where
    doStyle False _ attr = attr
    doStyle True  style attr = withStyle attr style

    doColor _ Nothing attr = attr
    doColor with (Just x) attr = with attr x

defaultFormatState :: FormatState
defaultFormatState = FormatState Nothing Nothing False False False False

-- | Parse mIRC encoded format characters and hide the control characters.
parseIrcText :: Text -> Image
parseIrcText = parseIrcText' False

-- | Parse mIRC encoded format characters and render the control characters
-- explicitly. This view is useful when inputting control characters to make
-- it clear where they are in the text.
parseIrcTextExplicit :: Text -> Image
parseIrcTextExplicit = parseIrcText' True

parseIrcText' :: Bool -> Text -> Image
parseIrcText' explicit = either (Vty.string defAttr) id
                      . parseOnly (pIrcLine explicit defaultFormatState)

data Segment = TextSegment Text | ControlSegment Char

pSegment :: Parser Segment
pSegment = TextSegment    <$> takeWhile1 (not . isControl)
       <|> ControlSegment <$> satisfy isControl

pIrcLine :: Bool -> FormatState -> Parser Image
pIrcLine explicit fmt =
  do seg <- option Nothing (Just <$> pSegment)
     case seg of
       Nothing -> return emptyImage
       Just (TextSegment txt) ->
           do rest <- pIrcLine explicit fmt
              return (text' (formatAttr fmt) txt Vty.<|> rest)
       Just (ControlSegment '\^C') ->
           do (numberText, colorNumbers) <- match pColorNumbers
              rest <- pIrcLine explicit (applyColors colorNumbers fmt)
              return $ if explicit
                         then Vty.char controlAttr 'C'
                              Vty.<|> text' defAttr numberText
                              Vty.<|> rest
                          else rest
       Just (ControlSegment c)
          -- always render control codes that we don't understand
          | isNothing mbFmt' || explicit ->
                do rest <- next
                   return (controlImage c Vty.<|> rest)
          | otherwise -> next
          where
            mbFmt' = applyControlEffect c fmt
            next = pIrcLine explicit (fromMaybe fmt mbFmt')

pColorNumbers :: Parser (Maybe Int, Maybe Int)
pColorNumbers = option (Nothing,Nothing) $
  do n <- pNumber
     m <- optional (Parse.char ',' *> pNumber)
     return (Just n,m)

  where
    pNumber = do d1 <- digit
                 ds <- option [] (return <$> digit)
                 return $! read (d1:ds)

optional :: Parser a -> Parser (Maybe a)
optional p = option Nothing (Just <$> p)

applyColors :: (Maybe Int, Maybe Int) -> FormatState -> FormatState
applyColors (fore, back) = aux fmtFore fore . aux fmtBack back
  where
    aux _ Nothing  = id
    aux l (Just x) = set l (mircColor x)

mircColor :: Int -> Maybe Color
mircColor  0 = Just (white                ) -- white
mircColor  1 = Just (black                ) -- black
mircColor  2 = Just (blue                 ) -- blue
mircColor  3 = Just (green                ) -- green
mircColor  4 = Just (red                  ) -- red
mircColor  5 = Just (rgbColor' 127 0 0    ) -- brown
mircColor  6 = Just (rgbColor' 156 0 156  ) -- purple
mircColor  7 = Just (rgbColor' 252 127 0  ) -- yellow
mircColor  8 = Just (yellow               ) -- yellow
mircColor  9 = Just (brightGreen          ) -- green
mircColor 10 = Just (cyan                 ) -- brightBlue
mircColor 11 = Just (brightCyan           ) -- brightCyan
mircColor 12 = Just (brightBlue           ) -- brightBlue
mircColor 13 = Just (rgbColor' 255 0 255  ) -- brightRed
mircColor 14 = Just (rgbColor' 127 127 127) -- brightBlack
mircColor 15 = Just (rgbColor' 210 210 210) -- brightWhite
mircColor  _ = Nothing

rgbColor' :: Int -> Int -> Int -> Color
rgbColor' = rgbColor -- fix the type to Int

applyControlEffect :: Char -> FormatState -> Maybe FormatState
applyControlEffect '\^B' = Just . over fmtBold not
applyControlEffect '\^O' = Just . const defaultFormatState
applyControlEffect '\^V' = Just . over fmtReverse not
applyControlEffect '\^]' = Just . over fmtItalic not
applyControlEffect '\^_' = Just . over fmtUnderline not
applyControlEffect _     = const Nothing

controlImage :: Char -> Image
controlImage = Vty.char controlAttr . controlName

controlAttr :: Attr
controlAttr = defAttr `withStyle` reverseVideo

controlName :: Char -> Char
controlName c = chr (ord '@' + ord c)
