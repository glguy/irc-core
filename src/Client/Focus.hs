{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Client.Focus
Description : Types for representing the current window being displayed
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

The client has a primary message window whose contents are determined
by a 'ClientFocus'. In order to provide different views of channels
the 'ClientSubfocus' breaks down channel focus into different subviews.
-}

module Client.Focus
  ( -- * Types
    ClientFocus(..)
  , ClientSubfocus(..)

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
  ) where

import           Control.Lens
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Irc.Identifier

-- | Currently focused window
data ClientFocus
 = Unfocused                      -- ^ No network
 | NetworkFocus !Text             -- ^ Network
 | ChannelFocus !Text !Identifier -- ^ Network Channel/Nick
  deriving (Eq,Show)

makePrisms ''ClientFocus

-- | Subfocus for a channel view
data ClientSubfocus
  = FocusMessages    -- ^ Show chat messages
  | FocusInfo        -- ^ Show channel metadata
  | FocusUsers       -- ^ Show user list
  | FocusMasks !Char -- ^ Show mask list for given mode
  deriving (Eq,Show)

makePrisms ''ClientSubfocus

-- | Unfocused first, followed by focuses sorted by network.
-- Within the same network the network focus comes first and
-- then the channels are ordered by channel identifier
instance Ord ClientFocus where
  compare Unfocused            Unfocused            = EQ
  compare (NetworkFocus x)     (NetworkFocus y    ) = compare x y
  compare (ChannelFocus x1 x2) (ChannelFocus y1 y2) = compare x1 y1 <> compare x2 y2

  compare Unfocused _         = LT
  compare _         Unfocused = GT

  compare (NetworkFocus x  ) (ChannelFocus y _) = compare x y <> LT
  compare (ChannelFocus x _) (NetworkFocus y  ) = compare x y <> GT

-- | Return the network associated with the current focus
focusNetwork :: ClientFocus -> Maybe Text {- ^ network -}
focusNetwork Unfocused = Nothing
focusNetwork (NetworkFocus network) = Just network
focusNetwork (ChannelFocus network _) = Just network
