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
  , palJoin
  , palPart
  , palModes
  , palUsrChg
  , palIgnore

  , paletteMap
  , modesPaletteFor

  -- * Defaults
  , defaultPalette
  ) where

import Control.Lens (makeLenses, ReifiedLens(Lens), ReifiedLens')
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import Graphics.Vty.Attributes
import Irc.Identifier

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
  , _palModes         :: Attr -- ^ mode lines
  , _palCModes        :: HashMap Char Attr -- ^ channel mode attributes
  , _palUModes        :: HashMap Char Attr -- ^ user mode attributes
  , _palSnomask       :: HashMap Char Attr -- ^ snotice mask attributes
  , _palJoin          :: Attr
  , _palPart          :: Attr
  , _palUsrChg       :: Attr
  , _palIgnore        :: Attr
  }
  deriving Show

makeLenses ''Palette

-- | Default UI colors
defaultPalette :: Palette
defaultPalette = Palette
  { _palNicks                   = defaultNickColorPalette
  , _palSelf                    = withForeColor defAttr brightWhite
  , _palSelfHighlight           = defAttr `withBackColor` brightYellow `withForeColor` black
  , _palTime                    = withForeColor defAttr brightBlack
  , _palMeta                    = metaNo
  , _palSigil                   = withForeColor defAttr brightYellow
  , _palLabel                   = withForeColor defAttr cyan
  , _palLatency                 = withForeColor defAttr green
  , _palWindowName              = withForeColor defAttr cyan
  , _palError                   = withForeColor (defAttr `withStyle` bold) red
  , _palTextBox                 = withForeColor defAttr brightBlack
  , _palActivity                = metaLo
  , _palMention                 = metaHi
  , _palCommand                 = withForeColor defAttr yellow
  , _palCommandReady            = withForeColor defAttr brightGreen
  , _palCommandPrefix           = withForeColor defAttr yellow
  , _palCommandError            = withForeColor defAttr red
  , _palCommandPlaceholder      = withForeColor defAttr brightBlack
  , _palCommandPrompt           = defAttr `withStyle` bold `withBackColor` green `withForeColor` white
  , _palWindowDivider           = withStyle defAttr reverseVideo
  , _palLineMarker              = withForeColor defAttr cyan
  , _palAway                    = withForeColor defAttr blue
  , _palMonospace               = defAttr
  , _palCModes                  = HashMap.fromList
    [ ('b', metaHi)
    , ('m', metaHi)
    , ('n', metaNo)
    , ('o', metaHi)
    , ('t', metaNo)
    , ('q', metaHi)  -- q is important whether it's quiet or owner.
    ]
  , _palUModes                  = HashMap.fromList
    [ ('i', metaNo)
    , ('w', metaNo)
    ]
  , _palSnomask                 = HashMap.empty
  , _palJoin                    = withForeColor defAttr brightGreen
  , _palPart                    = withForeColor defAttr brightRed
  , _palModes                   = metaLo
  , _palUsrChg                  = metaLo
  , _palIgnore                  = withForeColor defAttr white
  }
  where
    metaNo = withForeColor defAttr brightBlack
    metaLo = withForeColor defAttr brightBlue
    metaHi = defAttr `withStyle` bold `withBackColor` brightMagenta `withForeColor` black
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

modesPaletteFor :: Identifier -> Palette -> HashMap Char Attr
modesPaletteFor name
  | isChanPrefix $ Text.head $ idText name = _palCModes
  | otherwise = _palUModes
  where
    -- TODO: Don't hardcode this, query ISUPPORT.
    isChanPrefix c = c `elem` ("#&!+" :: String)

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
  , ("command-prompt"   , Lens palCommandPrompt)
  , ("window-divider"   , Lens palWindowDivider)
  , ("line-marker"      , Lens palLineMarker)
  , ("away"             , Lens palAway)
  , ("monospace"        , Lens palMonospace)
  , ("join"             , Lens palJoin)
  , ("part"             , Lens palPart)
  , ("user-change"      , Lens palUsrChg)
  , ("ignore"           , Lens palIgnore)
  ]
