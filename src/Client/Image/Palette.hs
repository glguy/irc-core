{-# Language TemplateHaskell #-}
{-|
Module      : Client.Image.Palette
Description : Palette of colors used to render the UI
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides names for all of the colors used in the UI.
-}
module Client.Image.Palette
  (
  -- * Palette type
    Palette(..)
  , palNicks
  , palTime
  , palMeta
  , palSigil
  , palLabel
  , palLatency
  , palWindowName
  , palError
  , palTextBox
  , palActivity
  , palMention

  -- * Defaults
  , defaultPalette
  ) where

import           Control.Lens
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Graphics.Vty.Attributes

-- | Color palette used for rendering the client UI
data Palette = Palette
  { _palNicks      :: Vector Color -- ^ colors for highlighting nicknames
  , _palTime       :: Attr -- ^ color of message timestamps
  , _palMeta       :: Attr -- ^ color of coalesced metadata
  , _palSigil      :: Attr -- ^ color of sigils (e.g. @+)
  , _palLabel      :: Attr -- ^ color of information labels
  , _palLatency    :: Attr -- ^ color of ping latency
  , _palWindowName :: Attr -- ^ color of window name
  , _palError      :: Attr -- ^ color of error messages
  , _palTextBox    :: Attr -- ^ color of textbox markers
  , _palActivity   :: Attr -- ^ color of window name with activity
  , _palMention    :: Attr -- ^ color of window name with mention
  }
  deriving Show

makeLenses ''Palette

-- | Default UI colors that look nice in my dark solarized color scheme
defaultPalette :: Palette
defaultPalette = Palette
  { _palNicks      = defaultNickColorPalette
  , _palTime       = withForeColor defAttr brightBlack
  , _palMeta       = withForeColor defAttr brightBlack
  , _palSigil      = withForeColor defAttr cyan
  , _palLabel      = withForeColor defAttr green
  , _palLatency    = withForeColor defAttr yellow
  , _palWindowName = withForeColor defAttr cyan
  , _palError      = withForeColor defAttr red
  , _palTextBox    = withForeColor defAttr brightBlack
  , _palActivity   = withForeColor defAttr green
  , _palMention    = withForeColor defAttr red
  }

-- | Default nick highlighting colors that look nice in my dark solarized
-- color scheme.
defaultNickColorPalette :: Vector Color
defaultNickColorPalette = Vector.fromList
  [cyan, magenta, green, yellow, blue,
   brightCyan, brightMagenta, brightGreen, brightBlue]
