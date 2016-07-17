{-# Language TemplateHaskell #-}

module Client.MircFormatting
  ( parseIrcText
  , parseIrcTextExplicit
  ) where

import Graphics.Vty hiding ((<|>))
import qualified Graphics.Vty as Vty
import Data.Attoparsec.Text as Parse
import Data.Char
import Data.Text (Text)
import Control.Applicative ((<|>))
import Control.Lens

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

parseIrcText :: Text -> Image
parseIrcText = parseIrcText' False

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
          | explicit -> do rest <- next
                           return (Vty.char controlAttr (controlName c) Vty.<|> rest)
          | otherwise -> next
          where
            next = pIrcLine explicit $ applyControlEffect c fmt

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

applyControlEffect :: Char -> FormatState -> FormatState
applyControlEffect '\^B' = over fmtBold not
applyControlEffect '\^O' = const defaultFormatState
applyControlEffect '\^V' = over fmtReverse not
applyControlEffect '\^]' = over fmtItalic not
applyControlEffect '\^_' = over fmtUnderline not
applyControlEffect _     = id

controlAttr :: Attr
controlAttr = defAttr `withStyle` reverseVideo

controlName :: Char -> Char
controlName c = chr (ord '@' + ord c)
