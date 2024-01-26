{-# LANGUAGE CPP, TemplateHaskell #-}

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
  , WindowsFilter(..)

  -- * Focus operations
  , parseFocus
  , focusNetwork
  , actualFocus
  , isPrefixOfFocus

  -- * Focus Prisms
  , _ChannelFocus
  , _NetworkFocus
  , _Unfocused
  ) where

import           Control.Lens (makePrisms, (<&>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Irc.Identifier (Identifier, idPrefix, mkId)

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
  | FocusInfo  !Text !Identifier       -- ^ Show channel metadata
  | FocusUsers !Text !Identifier       -- ^ Show channel user list
  | FocusMasks !Text !Identifier !Char -- ^ Show channel mask list for given mode
  | FocusWindows WindowsFilter -- ^ Show client windows
  | FocusPalette     -- ^ Show current palette
  | FocusMentions    -- ^ Show all mentions
  | FocusDigraphs    -- ^ Show all digraphs
  | FocusKeyMap      -- ^ Show key bindings
  | FocusHelp (Maybe Text) -- ^ Show help window with optional command
  | FocusRtsStats    -- ^ Show GHC RTS statistics
  | FocusIgnoreList  -- ^ Show ignored masks
  | FocusCert        -- ^ Show rendered certificate
  | FocusChanList !Text (Maybe Int) (Maybe Int) -- ^ Show channel list
  | FocusWho !Text -- ^ Show last reply to a WHO query
  deriving (Eq,Show)

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

-- | Returns what focus a subfocus is actually for.
actualFocus :: Subfocus -> Focus -> Focus
actualFocus sf = case sf of
  FocusInfo  net chan   -> const (ChannelFocus net chan)
  FocusUsers net chan   -> const (ChannelFocus net chan)
  FocusMasks net chan _ -> const (ChannelFocus net chan)
  FocusChanList net _ _ -> const (NetworkFocus net)
  FocusWho net          -> const (NetworkFocus net)
  _ -> id

-- | Parses a single focus name given a default network.
parseFocus ::
  Maybe Text {- ^ default network    -} ->
  String {- ^ @[network:]target@ -} ->
  Maybe Focus
parseFocus mbNet x =
  case break (==':') x of
    ("*","")     -> pure Unfocused
    (net,_:"")   -> pure (NetworkFocus (Text.pack net))
    (net,_:chan) -> pure (ChannelFocus (Text.pack net) (mkId (Text.pack chan)))
    (chan,"")    -> mbNet <&> \net ->
                    ChannelFocus net (mkId (Text.pack chan))

isPrefixOfFocus :: String -> Focus -> Bool
isPrefixOfFocus prefix focus = case break (==':') prefix of
  ("","")  -> True
  ("*","") -> focus == Unfocused
  (chan,"") -> case focus of 
    ChannelFocus _    chanF -> idPrefix (mkId $ Text.pack chan) chanF
    NetworkFocus netF       -> Text.isPrefixOf (Text.pack chan) netF
    Unfocused               -> False
  (net,_:chan) -> case focus of
    ChannelFocus netF chanF -> netF == Text.pack net && idPrefix (mkId $ Text.pack chan) chanF
    _ -> False

-- | Filter argument for 'FocusWindows'
data WindowsFilter
  = AllWindows     -- ^ no filter
  | NetworkWindows -- ^ only network windows
  | ChannelWindows -- ^ only channel windows
  | UserWindows    -- ^ only user windows
  deriving (Eq, Show)
