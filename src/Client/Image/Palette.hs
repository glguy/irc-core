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
  , NetworkPalette(..)

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
  , palWindowDivider
  , palLineMarker
  , palAway
  , palMonospace
  , palJoin
  , palPart
  , palModes
  , palUsrChg
  , palIgnore

  -- * Lenses (Network)
  , palCModes
  , palUModes
  , palSnomask
  , palIdOverride

  , paletteMap
  , unifyNetworkPalette

  -- * Defaults
  , defaultPalette
  , defaultNetworkPalette
  ) where

import Control.Lens (makeLenses, ReifiedLens(Lens), ReifiedLens')
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Vector (Vector)
import Graphics.Vty.Attributes
import Irc.Identifier

-- | Color palette used for rendering the client UI
data Palette = Palette
  { _palNicks         :: Vector Attr -- ^ highlighting identifiers
  , _palIdOverride    :: HashMap Identifier Attr -- ^ overrides for specific identifiers
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
  , _palWindowDivider :: Attr -- ^ Divider between split windows
  , _palLineMarker    :: Attr -- ^ Divider between new and old messages
  , _palAway          :: Attr -- ^ color of nickname when away
  , _palMonospace     :: Attr -- ^ rendering of monospace formatting text
  , _palModes         :: Attr -- ^ mode lines
  , _palJoin          :: Attr
  , _palPart          :: Attr
  , _palUsrChg        :: Attr
  , _palIgnore        :: Attr
  }
  deriving Show

data NetworkPalette = NetworkPalette
  { _palCModes        :: HashMap Char Attr -- ^ channel mode attributes
  , _palUModes        :: HashMap Char Attr -- ^ user mode attributes
  , _palSnomask       :: HashMap Char Attr -- ^ snotice mask attributes
  }
  deriving Show

makeLenses ''Palette
makeLenses ''NetworkPalette

-- | Default UI colors
defaultPalette :: Palette
defaultPalette = Palette
  { _palNicks              = defaultNickColorPalette
  , _palIdOverride         = HashMap.empty
  , _palSelf               = withForeColor defAttr brightWhite
  , _palSelfHighlight      = defAttr `withBackColor` brightYellow `withForeColor` black
  , _palTime               = withForeColor defAttr brightBlack
  , _palMeta               = metaNo
  , _palSigil              = defAttr `withStyle` bold `withForeColor` brightYellow
  , _palLabel              = withForeColor defAttr cyan
  , _palLatency            = withForeColor defAttr green
  , _palWindowName         = withForeColor defAttr brightCyan
  , _palError              = defAttr `withStyle` bold `withForeColor` red
  , _palTextBox            = withForeColor defAttr brightBlack
  , _palActivity           = metaLo
  , _palMention            = metaHi
  , _palCommand            = withForeColor defAttr yellow
  , _palCommandReady       = withForeColor defAttr brightGreen
  , _palCommandPrefix      = withForeColor defAttr yellow
  , _palCommandError       = withForeColor defAttr red
  , _palCommandPlaceholder = withForeColor defAttr brightBlack
  , _palWindowDivider      = withStyle defAttr reverseVideo
  , _palLineMarker         = withForeColor defAttr cyan
  , _palAway               = withForeColor defAttr blue
  , _palMonospace          = defAttr
  , _palJoin               = withForeColor defAttr brightGreen
  , _palPart               = withForeColor defAttr brightRed
  , _palModes              = metaLo
  , _palUsrChg             = metaLo
  , _palIgnore             = withForeColor defAttr white
  }
  where
    metaNo = withForeColor defAttr brightBlack
    metaLo = withForeColor defAttr brightBlue
    metaHi = defAttr `withStyle` bold `withBackColor` brightMagenta `withForeColor` black

defaultNetworkPalette :: NetworkPalette
defaultNetworkPalette = NetworkPalette
  { _palCModes = HashMap.empty
  , _palUModes = HashMap.empty
  , _palSnomask= HashMap.empty
  }
-- | Default nick highlighting colors that look nice in my dark solarized
-- color scheme.
defaultNickColorPalette :: Vector Attr
defaultNickColorPalette =
  withForeColor defAttr <$>
    [ Color240  18, Color240  19, Color240  20, Color240  21, Color240  22, Color240  23
    , Color240  24, Color240  25, Color240  26, Color240  27, Color240  28, Color240  29
    , Color240 192, Color240 193, Color240 194, Color240 195, Color240 196, Color240 197
    , Color240 198, Color240 199, Color240 200, Color240 201, Color240 202, Color240 203
    ]

-- | Combine one NetworkPalette with another.
unifyNetworkPalette :: NetworkPalette -> NetworkPalette -> NetworkPalette
unifyNetworkPalette defaults net = NetworkPalette
  { _palCModes     = HashMap.union (_palCModes net)     (_palCModes defaults)
  , _palUModes     = HashMap.union (_palUModes net)     (_palUModes defaults)
  , _palSnomask    = HashMap.union (_palSnomask net)    (_palSnomask defaults)
  } -- TODO: Replace the above with a nicer lens pattern later.

-- | List of palette entry names and lenses for accessing that component
-- of the palette.
paletteMap :: [(Text, ReifiedLens' Palette Attr)]
paletteMap =
  [ ("self"             , Lens palSelf)
  , ("self-highlight"   , Lens palSelfHighlight)
  , ("time"             , Lens palTime)
  , ("meta"             , Lens palMeta)
  , ("modes"            , Lens palModes)
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
  , ("window-divider"   , Lens palWindowDivider)
  , ("line-marker"      , Lens palLineMarker)
  , ("away"             , Lens palAway)
  , ("monospace"        , Lens palMonospace)
  , ("join"             , Lens palJoin)
  , ("part"             , Lens palPart)
  , ("user-change"      , Lens palUsrChg)
  , ("ignore"           , Lens palIgnore)
  ]
