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
import           Control.Applicative
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Functor.Alt ((<!>))
import           Data.Text (Text)
import           Graphics.Vty.Attributes

-- | Parse a text attribute. This value should be a sections with the @fg@ and/or
-- @bg@ attributes. Otherwise it should be a color entry that will be used
-- for the foreground color. An empty sections value will result in 'defAttr'
attrSpec :: ValueSpecs Attr
attrSpec = namedSpec "attr" $
           withForeColor defAttr <$> colorSpec
       <!> fullAttrSpec

fullAttrSpec :: ValueSpecs Attr
fullAttrSpec = sectionsSpec "full-attr" $
  do mbFg <- optSection' "fg"    "Foreground color" colorSpec
     mbBg <- optSection' "bg"    "Background color" colorSpec
     mbSt <- optSection' "style" "Terminal font style" stylesSpec
     return ( aux withForeColor mbFg
            $ aux withBackColor mbBg
            $ aux (foldl withStyle) mbSt
            $ defAttr)
  where
    aux f xs z = foldl f z xs


stylesSpec :: ValueSpecs [Style]
stylesSpec = oneOrList styleSpec

styleSpec :: ValueSpecs Style
styleSpec = namedSpec "style" $
      blink        <$ atomSpec "blink"
  <!> bold         <$ atomSpec "bold"
  <!> dim          <$ atomSpec "dim"
  <!> reverseVideo <$ atomSpec "reverse-video"
  <!> standout     <$ atomSpec "standout"
  <!> underline    <$ atomSpec "underline"


-- | Parse a color. Support formats are:
--
-- * Number between 0-255
-- * Name of color
-- * RGB values of color as a list
colorSpec :: ValueSpecs Color
colorSpec = namedSpec "color" (colorNumberSpec <!> colorNameSpec <!> rgbSpec)

colorNameSpec :: ValueSpecs Color
colorNameSpec = customSpec "color name" anyAtomSpec (`HashMap.lookup` namedColors)

-- | Match integers between 0 and 255 as Terminal colors.
colorNumberSpec :: ValueSpecs Color
colorNumberSpec = customSpec "terminal color" valuesSpec $ \i ->
  if      i <   0 then Nothing
  else if i <  16 then Just (ISOColor (fromInteger i))
  else if i < 256 then Just (Color240 (fromInteger (i - 16)))
  else Nothing

-- | Configuration section that matches 3 integers in the range 0-255
-- representing red, green, and blue values.
rgbSpec :: ValueSpecs Color
rgbSpec = customSpec "RGB" valuesSpec $ \rgb ->
  case rgb of
    [r,g,b] | valid r, valid g, valid b -> Just (rgbColor r g b)
    _                                   -> Nothing
  where
    valid x = 0 <= x && x < (256 :: Integer)

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
