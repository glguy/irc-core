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
  , winMessages
  , winUnread
  , winTotal
  , winMention
  , winMarker
  , winHideMeta

  -- * Window lines
  , WindowLine(..)
  , wlSummary
  , wlText
  , wlImage
  , wlFullImage
  , wlImportance
  , wlTimestamp

  -- * Window line importance
  , WindowLineImportance(..)

  -- * Window operations
  , emptyWindow
  , addToWindow
  , windowSeen
  , windowActivate
  , windowDeactivate
  ) where

import           Client.Image.PackedImage
import           Client.Message
import           Control.Lens
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Graphics.Vty.Image (Image)

-- | A single message to be displayed in a window
data WindowLine = WindowLine
  { _wlSummary    :: !IrcSummary  -- ^ Summary value
  , _wlText       :: {-# UNPACK #-} !Text -- ^ Searchable text form
  , _wlImage'     :: !Image'      -- ^ Normal rendered image
  , _wlFullImage' :: !Image'      -- ^ Detailed rendered image
  , _wlImportance :: !WindowLineImportance -- ^ Importance of message
  , _wlTimestamp  :: {-# UNPACK #-} !UTCTime
  }

data WindowLines
  = {-# UNPACK #-} !WindowLine :- WindowLines
  | Nil

-- | A 'Window' tracks all of the messages and metadata for a particular
-- message buffer.
data Window = Window
  { _winMessages :: !WindowLines   -- ^ Messages to display, newest first
  , _winMarker   :: !(Maybe Int)   -- ^ Location of line drawn to indicate newer messages
  , _winUnread   :: !Int           -- ^ Messages added since buffer was visible
  , _winTotal    :: !Int           -- ^ Messages in buffer
  , _winMention  :: !WindowLineImportance -- ^ Indicates an important event is unread
  , _winHideMeta :: !Bool          -- ^ Hide metadata messages
  }

data ActivityLevel = NoActivity | NormalActivity | HighActivity
  deriving (Eq, Ord, Read, Show)

-- | Flag for the important of a message being added to a window
data WindowLineImportance
  = WLBoring -- ^ Don't update unread count
  | WLNormal -- ^ Increment unread count
  | WLImportant -- ^ Increment unread count and set important flag
  deriving (Eq, Ord, Show, Read)

makeLenses ''Window
makeLenses ''WindowLine


-- | Lens for the '_wlImage' field viewed in unpacked form.
wlImage :: Lens' WindowLine Image
wlImage = wlImage' . _Image'
{-# INLINE wlImage #-}

-- | Lens for the '_wlFullImage' field viewed in unpacked form.
wlFullImage :: Lens' WindowLine Image
wlFullImage = wlFullImage' . _Image'
{-# INLINE wlFullImage #-}


-- | A window with no messages
emptyWindow :: Window
emptyWindow = Window
  { _winMessages = Nil
  , _winMarker   = Nothing
  , _winUnread   = 0
  , _winTotal    = 0
  , _winMention  = WLBoring
  , _winHideMeta = False
  }

-- | Adds a given line to a window as the newest message. Window's
-- unread count will be updated according to the given importance.
addToWindow :: WindowLine -> Window -> Window
addToWindow !msg !win = Window
    { _winMessages = msg :- view winMessages win
    , _winTotal    = view winTotal win + 1
    , _winMarker   = do i <- view winMarker win; return $! i+1
    , _winUnread   = if view wlImportance msg == WLBoring
                     then view winUnread win
                     else view winUnread win + 1
    , _winMention  = max (view winMention win) (view wlImportance msg)
    , _winHideMeta = view winHideMeta win
    }

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
