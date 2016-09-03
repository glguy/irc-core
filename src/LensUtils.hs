{-|
Module      : LensUtils
Description : Lens utility functions
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides functions that are useful with lenses.

-}
module LensUtils
  (
  -- * Strict update operations
    overStrict
  , setStrict

  -- * time lenses
  , zonedTimeLocalTime
  , localTimeTimeOfDay
  , localTimeDay
  ) where

import           Control.Lens
import           Data.Time
import           StrictUnit

-- | Modify the target of a 'Setter' with a function. The result
-- is strict in the results of applying the function. Strict version
-- of 'over'
overStrict :: LensLike ((,) StrictUnit) s t a b -> (a -> b) -> s -> t
overStrict l f = run . l (nur . f)
  where
    nur y = (y `seq` StrictUnit, y)
    run (StrictUnit,y) = y
{-# INLINE overStrict #-}

-- | Set a value strictly in the set value. Strict version of 'set'.
setStrict :: ASetter s t a b -> b -> s -> t
setStrict l x = set l $! x
{-# INLINE setStrict #-}

-- | 'Lens' to the 'LocalTime' component of a 'ZonedTime'
zonedTimeLocalTime :: Lens' ZonedTime LocalTime
zonedTimeLocalTime f (ZonedTime t z) = (ZonedTime ?? z) <$> f t
{-# INLINE zonedTimeLocalTime #-}

-- | 'Lens' to the 'TimeOfDay component of a 'LocalTime'.
localTimeTimeOfDay :: Lens' LocalTime TimeOfDay
localTimeTimeOfDay f (LocalTime d t) = LocalTime d <$> f t
{-# INLINE localTimeTimeOfDay #-}

-- | 'Lens' to the 'TimeOfDay component of a 'LocalTime'.
localTimeDay :: Lens' LocalTime Day
localTimeDay f (LocalTime d t) = (LocalTime ?? t) <$> f d
{-# INLINE localTimeDay #-}
