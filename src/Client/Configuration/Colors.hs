{-# Language OverloadedStrings #-}

{-|
Module      : Client.Configuration
Description : Client configuration format and operations
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module defines the top-level configuration information for the client.
-}

module Client.Configuration.Colors
  ( parseColors
  ) where

import           Client.IdentifierColors (defaultNickColorPalette)
import           Config
import           Config.FromConfig
import           Control.Exception
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Ratio
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Graphics.Vty.Attributes
import           System.IO.Unsafe

parseColors :: Value -> ConfigParser (Vector Color)
parseColors (List []) = failure "Empty color palette"
parseColors (List xs) = traverse parseColor (Vector.fromList xs)
parseColors (Atom "default") = return defaultNickColorPalette
parseColors _ = failure "Expected list of colors or default"

-- | Parse a color. Support formats are:
--
-- * Number between 0-255
-- * Name of color
-- * RGB values of color as a list
parseColor :: Value -> ConfigParser Color
parseColor v =
  case v of
    _ | Just i <- parseInteger v -> parseColorNumber i
    Atom a | Just c <- HashMap.lookup (atomName a) namedColors -> return c
    List [r,g,b]
      | Just r' <- parseInteger r
      , Just g' <- parseInteger g
      , Just b' <- parseInteger b ->
         parseRgb r' g' b'
    _ -> failure "Expected a color number, name, or RBG list"

parseColorNumber :: Integer -> ConfigParser Color
parseColorNumber i
  | i < 0 = failure "Negative color not supported"
  | i < 16 = return (ISOColor (fromInteger i))
  | i < 256 = return (Color240 (fromInteger (i - 16)))
  | otherwise = failure "Color value too high"

parseInteger :: Value -> Maybe Integer
parseInteger v =
  case v of
    Number _ i -> Just i
    Floating c e
      | denominator r == 1 -> Just (numerator r)
      where r = fromInteger c * 10^^e
    _ -> Nothing

parseRgb :: Integer -> Integer -> Integer -> ConfigParser Color
parseRgb r g b =
  case unsafePerformIO (try (evaluate (rgbColor r g b))) of
    Left (ErrorCallWithLocation msg _) -> failure (Text.pack msg)
    Right c -> return c

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
