{-# Language OverloadedStrings #-}
{-# Language ApplicativeDo #-}

{-|
Module      : Client.Configuration
Description : Client configuration format and operations
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module defines the top-level configuration information for the client.
-}

module Client.Configuration.Colors
  ( colorSpec
  , attrSpec
  ) where

import           Config.Schema
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Functor.Alt ((<!>))
import           Data.Text (Text)
import           Graphics.Vty.Attributes

-- | Parse a text attribute. This value should be a sections with the @fg@ and/or
-- @bg@ attributes. Otherwise it should be a color entry that will be used
-- for the foreground color. An empty sections value will result in 'defAttr'
attrSpec :: ValueSpec Attr
attrSpec = namedSpec "attr" $
           withForeColor defAttr <$> colorSpec
       <!> fullAttrSpec

fullAttrSpec :: ValueSpec Attr
fullAttrSpec = sectionsSpec "full-attr" $
  do mbFg <- optSection' "fg"    colorSpec "Foreground color"
     mbBg <- optSection' "bg"    colorSpec "Background color"
     mbSt <- optSection' "style" stylesSpec "Terminal font style"
     return ( aux withForeColor mbFg
            $ aux withBackColor mbBg
            $ aux (foldl withStyle) mbSt
            $ defAttr)
  where
    aux f xs z = foldl f z xs


stylesSpec :: ValueSpec [Style]
stylesSpec = oneOrList styleSpec

styleSpec :: ValueSpec Style
styleSpec = namedSpec "style" $
      blink        <$ atomSpec "blink"
  <!> bold         <$ atomSpec "bold"
  <!> dim          <$ atomSpec "dim"
  <!> italic       <$ atomSpec "italic"
  <!> reverseVideo <$ atomSpec "reverse-video"
  <!> standout     <$ atomSpec "standout"
  <!> strikethrough<$ atomSpec "strikethrough"
  <!> underline    <$ atomSpec "underline"


-- | Parse a color. Support formats are:
--
-- * Number between 0-255
-- * Name of color
-- * RGB values of color as a list
colorSpec :: ValueSpec Color
colorSpec = namedSpec "color" (colorNumberSpec <!> colorNameSpec <!> rgbSpec)

colorNameSpec :: ValueSpec Color
colorNameSpec = customSpec "color name" anyAtomSpec
              $ \name -> case HashMap.lookup name namedColors of
                           Nothing -> Left "unknown color"
                           Just c  -> Right c

-- | Match integers between 0 and 255 as Terminal colors.
colorNumberSpec :: ValueSpec Color
colorNumberSpec = customSpec "terminal color" anySpec $ \i ->
  if      i <   0 then Left "minimum color is 0"
  else if i <  16 then Right (ISOColor (fromInteger i))
  else if i < 256 then Right (Color240 (fromInteger (i - 16)))
  else Left "maximum color is 255"

-- | Configuration section that matches 3 integers in the range 0-255
-- representing red, green, and blue values.
rgbSpec :: ValueSpec Color
rgbSpec = customSpec "RGB" anySpec $ \rgb ->
  case rgb of
    [r,g,b] -> rgbColor <$> valid r <*> valid g <*> valid b
    _ -> Left "expected 3 numbers"
  where
    valid x
      | x < 0     = Left "minimum color value is 0"
      | x < 256   = Right (x :: Integer)
      | otherwise = Left "maximum color value is 255"

namedColors :: HashMap Text Color
namedColors = HashMap.fromList
  [ ("black"         , black        )
  , ("red"           , red          )
  , ("green"         , green        )
  , ("yellow"        , yellow       )
  , ("blue"          , blue         )
  , ("magenta"       , magenta      )
  , ("cyan"          , cyan         )
  , ("white"         , white        )
  , ("bright-black"  , brightBlack  )
  , ("bright-red"    , brightRed    )
  , ("bright-green"  , brightGreen  )
  , ("bright-yellow" , brightYellow )
  , ("bright-blue"   , brightBlue   )
  , ("bright-magenta", brightMagenta)
  , ("bright-cyan"   , brightCyan   )
  , ("bright-white"  , brightWhite  )
  ]
