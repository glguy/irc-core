{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Client.State.Focus
Description : Types for representing the current window being displayed
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

The client has a primary message window whose contents are determined
by a 'Focus'. In order to provide different views of channels
the 'Subfocus' breaks down channel focus into different subviews.
-}

module Client.State.Focus
  ( -- * Types
    Focus(..)
  , Subfocus(..)

  -- * Focus operations
  , focusNetwork

  -- * Focus Prisms
  , _ChannelFocus
  , _NetworkFocus
  , _Unfocused

  -- * Subfocus Prisms
  , _FocusMessages
  , _FocusInfo
  , _FocusUsers
  , _FocusMasks
  , _FocusWindows
  ) where

import           Control.Lens
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Irc.Identifier

-- | Currently focused window
data Focus
 = Unfocused                      -- ^ No network
 | NetworkFocus !Text             -- ^ Network
 | ChannelFocus !Text !Identifier -- ^ Network Channel/Nick
  deriving (Eq,Show)

makePrisms ''Focus

-- | Subfocus view
data Subfocus
  = FocusMessages    -- ^ Show messages
  | FocusInfo        -- ^ Show channel metadata
  | FocusUsers       -- ^ Show channel user list
  | FocusMasks !Char -- ^ Show channel mask list for given mode
  | FocusWindows     -- ^ Show client windows
  | FocusPalette     -- ^ Show current palette
  | FocusHelp (Maybe Text) -- ^ Show help window with optional command
  deriving (Eq,Show)

makePrisms ''Subfocus

-- | Unfocused first, followed by focuses sorted by network.
-- Within the same network the network focus comes first and
-- then the channels are ordered by channel identifier
instance Ord Focus where
  compare Unfocused            Unfocused            = EQ
  compare (NetworkFocus x)     (NetworkFocus y    ) = compare x y
  compare (ChannelFocus x1 x2) (ChannelFocus y1 y2) = compare x1 y1 <> compare x2 y2

  compare Unfocused _         = LT
  compare _         Unfocused = GT

  compare (NetworkFocus x  ) (ChannelFocus y _) = compare x y <> LT
  compare (ChannelFocus x _) (NetworkFocus y  ) = compare x y <> GT

-- | Return the network associated with the current focus
focusNetwork :: Focus -> Maybe Text {- ^ network -}
focusNetwork Unfocused = Nothing
focusNetwork (NetworkFocus network) = Just network
focusNetwork (ChannelFocus network _) = Just network
