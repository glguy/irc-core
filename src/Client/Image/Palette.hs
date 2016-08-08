{-|
Module      : Client.Image.Palette
Description : Palette of colors used to render the UI
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides names for all of the colors used in the UI.
-}
module Client.Image.Palette
  ( Palette(..)
  , defaultPalette
  ) where

import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Graphics.Vty.Attributes

data Palette = Palette
  { palNicks      :: Vector Color -- ^ colors for highlighting nicknames
  , palTime       :: Attr -- ^ color of message timestamps
  , palMeta       :: Attr -- ^ color of coalesced metadata
  , palSigil      :: Attr -- ^ color of sigils (e.g. @+)
  , palLabel      :: Attr -- ^ color of information labels
  , palLatency    :: Attr -- ^ color of ping latency
  , palWindowName :: Attr -- ^ color of window name
  }
  deriving Show

defaultPalette :: Palette
defaultPalette = Palette
  { palNicks      = defaultNickColorPalette
  , palTime       = withForeColor defAttr brightBlack
  , palMeta       = withForeColor defAttr brightBlack
  , palSigil      = withForeColor defAttr cyan
  , palLabel      = withForeColor defAttr green
  , palLatency    = withForeColor defAttr yellow
  , palWindowName = withForeColor defAttr cyan
  }

defaultNickColorPalette :: Vector Color
defaultNickColorPalette = Vector.fromList
  [cyan, magenta, green, yellow, blue,
   brightCyan, brightMagenta, brightGreen, brightBlue]
