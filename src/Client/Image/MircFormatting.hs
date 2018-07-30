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
  , parseIrcText'
  , plainText
  , controlImage
  , mircColor
  ) where

import           Client.Image.PackedImage as I
import           Control.Applicative ((<|>))
import           Control.Lens
import           Data.Attoparsec.Text as Parse
import           Data.Bits
import           Data.Char
import           Data.Maybe
import           Data.Text (Text)
import           Graphics.Vty.Attributes

makeLensesFor
  [ ("attrForeColor", "foreColorLens")
  , ("attrBackColor", "backColorLens")
  , ("attrStyle"    , "styleLens"    )]
  ''Attr

-- | Parse mIRC encoded format characters and hide the control characters.
parseIrcText :: Text -> Image'
parseIrcText = parseIrcText' False

-- | Parse mIRC encoded format characters and render the control characters
-- explicitly. This view is useful when inputting control characters to make
-- it clear where they are in the text.
parseIrcText' :: Bool -> Text -> Image'
parseIrcText' explicit = either plainText id
                       . parseOnly (pIrcLine explicit defAttr)

data Segment = TextSegment Text | ControlSegment Char

pSegment :: Parser Segment
pSegment = TextSegment    <$> takeWhile1 (not . isControl)
       <|> ControlSegment <$> satisfy isControl

pIrcLine :: Bool -> Attr -> Parser Image'
pIrcLine explicit fmt =
  do seg <- option Nothing (Just <$> pSegment)
     case seg of
       Nothing -> return mempty
       Just (TextSegment txt) ->
           do rest <- pIrcLine explicit fmt
              return (text' fmt txt <> rest)
       Just (ControlSegment '\^C') ->
           do (numberText, colorNumbers) <- match pColorNumbers
              rest <- pIrcLine explicit (applyColors colorNumbers fmt)
              return $ if explicit
                         then controlImage '\^C'
                              <> text' defAttr numberText
                              <> rest
                          else rest
       Just (ControlSegment c)
          -- always render control codes that we don't understand
          | isNothing mbFmt' || explicit ->
                do rest <- next
                   return (controlImage c <> rest)
          | otherwise -> next
          where
            mbFmt' = applyControlEffect c fmt
            next   = pIrcLine explicit (fromMaybe fmt mbFmt')

pColorNumbers :: Parser (Maybe (Color, Maybe Color))
pColorNumbers = option Nothing $
  do n       <- pNumber
     Just fc <- pure (mircColor n)
     bc      <- optional $
                  do m       <- Parse.char ',' *> pNumber
                     Just bc <- pure (mircColor m)
                     pure bc
     return (Just (fc,bc))

  where
    pNumber = do d1 <- digit
                 ds <- option [] (return <$> digit)
                 return $! read (d1:ds)

optional :: Parser a -> Parser (Maybe a)
optional p = option Nothing (Just <$> p)

applyColors :: (Maybe (Color, Maybe Color)) -> Attr -> Attr
applyColors Nothing = set foreColorLens Default
                    . set backColorLens Default
applyColors (Just (c1, Nothing)) = set foreColorLens (SetTo c1) -- preserve background
applyColors (Just (c1, Just c2)) = set foreColorLens (SetTo c1)
                                 . set backColorLens (SetTo c2)

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

applyControlEffect :: Char -> Attr -> Maybe Attr
applyControlEffect '\^B' attr = Just $! toggleStyle bold attr
applyControlEffect '\^V' attr = Just $! toggleStyle reverseVideo attr
applyControlEffect '\^_' attr = Just $! toggleStyle underline attr
applyControlEffect '\^]' attr = Just $! toggleStyle italic attr
applyControlEffect '\^O' _    = Just defAttr
applyControlEffect _     _    = Nothing

toggleStyle :: Style -> Attr -> Attr
toggleStyle s1 = over styleLens $ \old ->
  case old of
    SetTo s2 -> SetTo (xor s1 s2)
    _        -> SetTo s1

-- | Safely render a control character.
controlImage :: Char -> Image'
controlImage = I.char attr . controlName
  where
    attr          = withStyle defAttr reverseVideo
    controlName c
      | c < '\128' = chr (0x40 `xor` ord c)
      | otherwise  = '!'

-- | Render a 'String' with default attributes and replacing all of the
-- control characters with reverse-video letters corresponding to caret
-- notation.
plainText :: String -> Image'
plainText "" = mempty
plainText xs =
  case break isControl xs of
    (first, ""       ) -> I.string defAttr first
    (first, cntl:rest) -> I.string defAttr first <>
                          controlImage cntl <>
                          plainText rest
