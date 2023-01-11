{-# Language TemplateHaskell, OverloadedLists, OverloadedStrings #-}
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

  -- * Lenses
  , palNicks
  , palSelf
  , palSelfHighlight
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
  , palCommand
  , palCommandReady
  , palCommandPlaceholder
  , palCommandPrefix
  , palCommandError
  , palCommandPrompt
  , palWindowDivider
  , palLineMarker
  , palCModes
  , palUModes
  , palSnomask
  , palAway
  , palMonospace

  , paletteMap

  -- * Defaults
  , defaultPalette
  ) where

import Control.Lens (makeLenses, ReifiedLens(Lens), ReifiedLens')
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Vector (Vector)
import Graphics.Vty.Attributes

-- | Color palette used for rendering the client UI
data Palette = Palette
  { _palNicks         :: Vector Attr -- ^ highlighting nicknames
  , _palSelf          :: Attr -- ^ own nickname(s)
  , _palSelfHighlight :: Attr -- ^ own nickname(s) in mentions
  , _palTime          :: Attr -- ^ message timestamps
  , _palMeta          :: Attr -- ^ coalesced metadata
  , _palSigil         :: Attr -- ^ sigils (e.g. @+)
  , _palLabel         :: Attr -- ^ information labels
  , _palLatency       :: Attr -- ^ ping latency
  , _palWindowName    :: Attr -- ^ window name
  , _palError         :: Attr -- ^ error messages
  , _palTextBox       :: Attr -- ^ textbox markers
  , _palActivity      :: Attr -- ^ window name with activity
  , _palMention       :: Attr -- ^ window name with mention
  , _palCommand       :: Attr -- ^ known command
  , _palCommandReady  :: Attr -- ^ known command with complete arguments
  , _palCommandPrefix :: Attr -- ^ prefix of known command
  , _palCommandError  :: Attr -- ^ unknown command
  , _palCommandPlaceholder :: Attr -- ^ command argument placeholder
  , _palCommandPrompt :: Attr -- ^ Command input prefix @CMD:@
  , _palWindowDivider :: Attr -- ^ Divider between split windows
  , _palLineMarker    :: Attr -- ^ Divider between new and old messages
  , _palAway          :: Attr -- ^ color of nickname when away
  , _palMonospace     :: Attr -- ^ rendering of monospace formatting text
  , _palCModes        :: HashMap Char Attr -- ^ channel mode attributes
  , _palUModes        :: HashMap Char Attr -- ^ user mode attributes
  , _palSnomask       :: HashMap Char Attr -- ^ snotice mask attributes
  }
  deriving Show

makeLenses ''Palette

-- | Default UI colors that look nice in my dark solarized color scheme
defaultPalette :: Palette
defaultPalette = Palette
  { _palNicks                   = defaultNickColorPalette
  , _palSelf                    = withForeColor defAttr red
  , _palSelfHighlight           = withForeColor defAttr red
  , _palTime                    = withForeColor defAttr brightBlack
  , _palMeta                    = withForeColor defAttr brightBlack
  , _palSigil                   = withForeColor defAttr cyan
  , _palLabel                   = withForeColor defAttr green
  , _palLatency                 = withForeColor defAttr yellow
  , _palWindowName              = withForeColor defAttr cyan
  , _palError                   = withForeColor defAttr red
  , _palTextBox                 = withForeColor defAttr brightBlack
  , _palActivity                = withForeColor defAttr green
  , _palMention                 = withForeColor defAttr red
  , _palCommand                 = withForeColor defAttr yellow
  , _palCommandReady            = withForeColor defAttr green
  , _palCommandPrefix           = withForeColor defAttr blue
  , _palCommandError            = withForeColor defAttr red
  , _palCommandPlaceholder      = withStyle defAttr reverseVideo
  , _palCommandPrompt           = defAttr `withStyle` bold `withBackColor` green `withForeColor` white
  , _palWindowDivider           = withStyle defAttr reverseVideo
  , _palLineMarker              = defAttr
  , _palAway                    = withForeColor defAttr brightBlack
  , _palMonospace               = defAttr
  , _palCModes                  = HashMap.empty
  , _palUModes                  = HashMap.empty
  , _palSnomask                 = HashMap.empty
  }

-- | Default nick highlighting colors that look nice in my dark solarized
-- color scheme.
defaultNickColorPalette :: Vector Attr
defaultNickColorPalette =
  withForeColor defAttr <$>
    [cyan, magenta, green, yellow, blue,
     brightCyan, brightMagenta, brightGreen, brightBlue]

-- | List of palette entry names and lenses for accessing that component
-- of the palette.
paletteMap :: [(Text, ReifiedLens' Palette Attr)]
paletteMap =
  [ ("self"             , Lens palSelf)
  , ("self-highlight"   , Lens palSelfHighlight)
  , ("time"             , Lens palTime)
  , ("meta"             , Lens palMeta)
  , ("sigil"            , Lens palSigil)
  , ("label"            , Lens palLabel)
  , ("latency"          , Lens palLatency)
  , ("window-name"      , Lens palWindowName)
  , ("error"            , Lens palError)
  , ("textbox"          , Lens palTextBox)
  , ("activity"         , Lens palActivity)
  , ("mention"          , Lens palMention)
  , ("command"          , Lens palCommand)
  , ("command-ready"    , Lens palCommandReady)
  , ("command-placeholder", Lens palCommandPlaceholder)
  , ("command-prefix"   , Lens palCommandPrefix)
  , ("command-error"    , Lens palCommandError)
  , ("command-prompt"   , Lens palCommandPrompt)
  , ("window-divider"   , Lens palWindowDivider)
  , ("line-marker"      , Lens palLineMarker)
  , ("away"             , Lens palAway)
  , ("monospace"        , Lens palMonospace)
  ]
