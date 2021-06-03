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
  , mircColors
  ) where

import           Client.Image.PackedImage as I
import           Client.Image.Palette (Palette, palMonospace)
import           Control.Applicative ((<|>), empty)
import           Control.Lens
import           Data.Attoparsec.Text as Parse
import           Data.Bits
import           Data.Char
import           Data.Maybe
import           Data.Text (Text)
import           Graphics.Vty.Attributes
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Numeric (readHex)

makeLensesFor
  [ ("attrForeColor", "foreColorLens")
  , ("attrBackColor", "backColorLens")
  , ("attrStyle"    , "styleLens"    )]
  ''Attr

-- | Parse mIRC encoded format characters and hide the control characters.
parseIrcText :: Palette -> Text -> Image'
parseIrcText = parseIrcText' False

-- | Parse mIRC encoded format characters and render the control characters
-- explicitly. This view is useful when inputting control characters to make
-- it clear where they are in the text.
parseIrcText' :: Bool -> Palette -> Text -> Image'
parseIrcText' explicit pal
  = either plainText id
  . parseOnly (pIrcLine pal explicit False defAttr)

data Segment = TextSegment Text | ControlSegment Char

pSegment :: Parser Segment
pSegment = TextSegment    <$> takeWhile1 (not . isControl)
       <|> ControlSegment <$> satisfy isControl

pIrcLine :: Palette -> Bool -> Bool -> Attr -> Parser Image'
pIrcLine pal explicit mono fmt =
  do seg <- option Nothing (Just <$> pSegment)
     case seg of
       Nothing -> return mempty
       Just (TextSegment txt) ->
           do rest <- pIrcLine pal explicit mono fmt
              return (text' fmt txt <> rest)
       Just (ControlSegment '\^C') ->
           do (numberText, colorNumbers) <- match (pColorNumbers pColorNumber)
              rest <- pIrcLine pal explicit mono (applyColors colorNumbers fmt)
              return $ if explicit
                         then controlImage '\^C'
                              <> text' defAttr numberText
                              <> rest
                          else rest
       Just (ControlSegment '\^D') ->
           do (numberText, colorNumbers) <- match (pColorNumbers pColorHex)
              rest <- pIrcLine pal explicit mono (applyColors colorNumbers fmt)
              return $ if explicit
                         then controlImage '\^D'
                              <> text' defAttr numberText
                              <> rest
                          else rest
       Just (ControlSegment '\^Q')
         | explicit -> (controlImage '\^Q' <>) <$> rest
         | otherwise -> rest
         where
           rest = pIrcLine pal explicit (not mono)
                    (if mono then defAttr else view palMonospace pal)
       Just (ControlSegment c)
          -- always render control codes that we don't understand
          | isNothing mbFmt' || explicit ->
                do rest <- next
                   return (controlImage c <> rest)
          | otherwise -> next
          where
            mbFmt' = applyControlEffect c fmt
            next   = pIrcLine pal explicit mono (fromMaybe fmt mbFmt')

pColorNumbers ::
  Parser (MaybeDefault Color) ->
  Parser (Maybe (MaybeDefault Color, Maybe (MaybeDefault Color)))
pColorNumbers color = option Nothing $
  do fc <- color
     bc <- optional (Parse.char ',' *> color)
     return (Just (fc,bc))

pColorNumber :: Parser (MaybeDefault Color)
pColorNumber =
  do d1 <- digit
     ds <- option [] (return <$> digit)
     case mircColor (read (d1:ds)) of
       Just c -> pure c
       Nothing -> empty

pColorHex :: Parser (MaybeDefault Color)
pColorHex = SetTo <$> (rgbColor' <$> p <*> p <*> p)
  where
    p = do x <- satisfy isHexDigit
           y <- satisfy isHexDigit
           pure (fst (head (readHex [x,y])))

optional :: Parser a -> Parser (Maybe a)
optional p = option Nothing (Just <$> p)

applyColors :: Maybe (MaybeDefault Color, Maybe (MaybeDefault Color)) -> Attr -> Attr
applyColors Nothing = set foreColorLens Default
                    . set backColorLens Default
applyColors (Just (c1, Nothing)) = set foreColorLens c1 -- preserve background
applyColors (Just (c1, Just c2)) = set foreColorLens c1
                                 . set backColorLens c2

mircColor :: Int -> Maybe (MaybeDefault Color)
mircColor 99 = Just Default
mircColor i  = SetTo <$> mircColors Vector.!? i

mircColors :: Vector Color
mircColors =
  Vector.fromList $
    [ white                 -- white
    , black                 -- black
    , blue                  -- blue
    , green                 -- green
    , red                   -- red
    , rgbColor' 127 0 0     -- brown
    , rgbColor' 156 0 156   -- purple
    , rgbColor' 252 127 0   -- yellow
    , yellow                -- yellow
    , brightGreen           -- green
    , cyan                  -- brightBlue
    , brightCyan            -- brightCyan
    , brightBlue            -- brightBlue
    , rgbColor' 255 0 255   -- brightRed
    , rgbColor' 127 127 127 -- brightBlack
    , rgbColor' 210 210 210 -- brightWhite
    ] ++
    map (Color240 . subtract 16) [ -- https://modern.ircdocs.horse/formatting.html#colors-16-98
      052, 094, 100, 058,
      022, 029, 023, 024, 017, 054, 053, 089, 088, 130,
      142, 064, 028, 035, 030, 025, 018, 091, 090, 125,
      124, 166, 184, 106, 034, 049, 037, 033, 019, 129,
      127, 161, 196, 208, 226, 154, 046, 086, 051, 075,
      021, 171, 201, 198, 203, 215, 227, 191, 083, 122,
      087, 111, 063, 177, 207, 205, 217, 223, 229, 193,
      157, 158, 159, 153, 147, 183, 219, 212, 016, 233,
      235, 237, 239, 241, 244, 247, 250, 254, 231 ]

rgbColor' :: Int -> Int -> Int -> Color
rgbColor' = rgbColor -- fix the type to Int

applyControlEffect :: Char -> Attr -> Maybe Attr
applyControlEffect '\^B' attr = Just $! toggleStyle bold attr
applyControlEffect '\^V' attr = Just $! toggleStyle reverseVideo attr
applyControlEffect '\^_' attr = Just $! toggleStyle underline attr
applyControlEffect '\^^' attr = Just $! toggleStyle strikethrough attr
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
