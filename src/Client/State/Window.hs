{-# Language MultiParamTypeClasses, BangPatterns, TemplateHaskell #-}

{-|
Module      : Client.State.Window
Description : Types and operations for managing message buffers.
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module defines types and operations used to store messages for display
in the client's buffers.
-}

module Client.State.Window
  (
  -- * Windows
    Window(..)
  , winName
  , winMessages
  , winUnread
  , winTotal
  , winMention
  , winMarker
  , winHideMeta
  , winHidden
  , winActivityFilter

  -- * Window lines
  , WindowLines(..)
  , WindowLine(..)
  , wlSummary
  , wlText
  , wlPrefix
  , wlImage
  , wlFullImage
  , wlImportance
  , wlTimestamp

  -- * Window line importance
  , ActivityFilter(..)
  , WindowLineImportance(..)
  , activityFilterStrings
  , applyActivityFilter
  , readActivityFilter

  -- * Window operations
  , emptyWindow
  , addToWindow
  , windowSeen
  , windowActivate
  , windowDeactivate
  , windowClear

    -- * Packed time
  , PackedTime
  , packZonedTime
  , unpackUTCTime
  , unpackTimeOfDay
  ) where

import Client.Image.PackedImage (Image', imageText)
import Client.Message (IrcSummary)
import Control.Lens (Lens', view, to, from, non, set, makeLenses, Each(..), Getter)
import Control.Monad ((<$!>))
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.Text.Lazy (Text)
import Data.Time
import Data.Word (Word64)
import Data.List (elemIndex)

-- | A single message to be displayed in a window.
-- The normal message line consists of the image prefix
-- and the image. This allows line wrapping to be applied
-- separately to the image and prefix so that wrapped
-- messages can fall to the right side of the prefix.
data WindowLine = WindowLine
  { _wlSummary    :: !IrcSummary  -- ^ Summary value
  , _wlPrefix     :: !Image'      -- ^ Normal rendered image prefix
  , _wlImage      :: !Image'      -- ^ Normal rendered image
  , _wlFullImage  :: !Image'      -- ^ Detailed rendered image
  , _wlImportance :: !WindowLineImportance -- ^ Importance of message
  , _wlTimestamp  :: {-# UNPACK #-} !PackedTime
  }

newtype PackedTime = PackedTime Word64

data WindowLines
  = {-# UNPACK #-} !WindowLine :- WindowLines
  | Nil

-- | A 'Window' tracks all of the messages and metadata for a particular
-- message buffer.
data Window = Window
  { _winName'    :: !Char          -- ^ Shortcut name (or NUL)
  , _winMessages :: !WindowLines   -- ^ Messages to display, newest first
  , _winMarker   :: !(Maybe Int)   -- ^ Location of line drawn to indicate newer messages
  , _winUnread   :: !Int           -- ^ Messages added since buffer was visible
  , _winTotal    :: !Int           -- ^ Messages in buffer
  , _winMention  :: !WindowLineImportance -- ^ Indicates an important event is unread
  , _winHideMeta :: !Bool          -- ^ Hide metadata messages
  , _winHidden   :: !Bool          -- ^ Remove from jump rotation
  , _winActivityFilter :: !ActivityFilter -- ^ Filters for activity
  }

data ActivityLevel = NoActivity | NormalActivity | HighActivity
  deriving (Eq, Ord, Read, Show)

-- | Flag for the important of a message being added to a window
data WindowLineImportance
  = WLBoring -- ^ Don't update unread count
  | WLNormal -- ^ Increment unread count
  | WLImportant -- ^ Increment unread count and set important flag
  deriving (Eq, Ord, Show, Read, Enum)

data ActivityFilter
  = AFSilent
  | AFQuieter
  | AFQuiet
  | AFImpOnly
  | AFLoud
  | AFLouder
  deriving (Eq, Ord, Enum)

activityFilterStrings :: [String]
activityFilterStrings = ["silent", "quieter", "quiet", "imponly", "loud", "louder"]

applyActivityFilter :: ActivityFilter -> WindowLineImportance -> WindowLineImportance
applyActivityFilter AFSilent  _           = WLBoring
applyActivityFilter AFQuieter WLNormal    = WLBoring
applyActivityFilter AFQuieter WLImportant = WLNormal
applyActivityFilter AFImpOnly WLNormal    = WLBoring
applyActivityFilter AFQuiet   WLImportant = WLNormal
applyActivityFilter AFLouder  WLNormal    = WLImportant
applyActivityFilter _ etc = etc

instance Show ActivityFilter where
  show af = activityFilterStrings !! fromEnum af

readActivityFilter :: String -> Maybe ActivityFilter
readActivityFilter s = toEnum <$> elemIndex s activityFilterStrings

makeLenses ''Window
makeLenses ''WindowLine

winName :: Lens' Window (Maybe Char)
winName = winName' . from (non '\0')

wlText :: Getter WindowLine Text
wlText = wlFullImage . to imageText

-- | A window with no messages
emptyWindow :: Window
emptyWindow = Window
  { _winName'    = '\0'
  , _winMessages = Nil
  , _winMarker   = Nothing
  , _winUnread   = 0
  , _winTotal    = 0
  , _winMention  = WLBoring
  , _winHideMeta = False
  , _winHidden   = False
  , _winActivityFilter   = AFLoud
  }

windowClear :: Window -> Window
windowClear w = w
  { _winMessages = Nil
  , _winMarker = Nothing
  , _winUnread = 0
  , _winTotal = 0
  , _winMention  = WLBoring
  }

-- | Adds a given line to a window as the newest message. Window's
-- unread count will be updated according to the given importance.
-- Additionally returns True if this window becomes important as a result of this line.
addToWindow :: WindowLine -> Window -> (Window, Bool)
addToWindow !msg !win = (win', nowImportant)
    where
      win' = win
        { _winMessages = msg :- view winMessages win
        , _winTotal    = view winTotal win + 1
        , _winMarker   = (+1) <$!> view winMarker win
        , _winUnread   = if msgImportance == WLBoring
                         then view winUnread win
                         else view winUnread win + 1
        , _winMention  = max oldMention msgImportance
        , _winHideMeta = view winHideMeta win
        }
      oldMention = view winMention win
      nowImportant = oldMention < WLImportant && msgImportance >= WLImportant
      msgImportance = applyActivityFilter (view winActivityFilter win) (view wlImportance msg)

-- | Update the window clearing the unread count and important flag.
windowSeen :: Window -> Window
windowSeen = set winUnread 0
           . set winMention WLBoring


-- | Update the window when it first becomes active. If only /boring/
-- messages have been added since last time the marker will be hidden.
windowActivate :: Window -> Window
windowActivate win
  | view winUnread win == 0 = set winMarker Nothing win
  | otherwise               = win


-- | Update the window when it becomes inactive. This resets the activity
-- marker to the bottom of the window.
windowDeactivate :: Window -> Window
windowDeactivate = set winMarker (Just 0)


instance Each WindowLines WindowLines WindowLine WindowLine where
  each _ Nil = pure Nil
  each f (x :- xs) = (:-) <$> f x <*> each f xs

------------------------------------------------------------------------

-- Field   Range   Bits Start
-- year:     0..   33     31
-- month:    1..12 4      27
-- day:      1..31 5      22
-- hour:     0..23 5      17
-- minute:   0..60 6      11
-- second:   0..61 6       5
-- offset: -12..14 5       0

field :: Num a => PackedTime -> Int -> Int -> a
field (PackedTime x) off sz = fromIntegral ((x `shiftR` off) .&. (2^sz-1))
{-# INLINE field #-}

packField :: Int -> Int -> Word64
packField off val = fromIntegral val `shiftL` off

packZonedTime :: ZonedTime -> PackedTime
packZonedTime (ZonedTime (LocalTime (ModifiedJulianDay d) (TimeOfDay h m s)) z)
  = PackedTime
  $ packField 17 h .|.
    packField 11 m .|.
    packField  5 (floor s) .|.
    packField 22 (fromInteger d) .|.
    packField  0 (timeZoneMinutes z `div` 60 + 12)

unpackTimeOfDay :: PackedTime -> TimeOfDay
unpackTimeOfDay !x = TimeOfDay h m s
  where
    h = field x 17 5
    m = field x 11 6
    s = field x  5 6

unpackLocalTime :: PackedTime -> LocalTime
unpackLocalTime !x = LocalTime d t
  where
    d = ModifiedJulianDay (field x 22 42)
    t = unpackTimeOfDay x

unpackUTCTime :: PackedTime -> UTCTime
unpackUTCTime = zonedTimeToUTC . unpackZonedTime

unpackZonedTime :: PackedTime -> ZonedTime
unpackZonedTime !x = ZonedTime t z
  where
    z = minutesToTimeZone ((field x 0 5 - 12) * 60)
    t = unpackLocalTime x
