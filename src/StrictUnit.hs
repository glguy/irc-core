{-|
Module      : StrictUnit
Description : Strict unit type
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a unit type where the 'Monoid' and 'Semigroup'
instances are strict in their append operations. This is a helper
module for "LensUtils".

-}

module StrictUnit
  ( StrictUnit(..)
  ) where

import Data.Semigroup (stimes, stimesIdempotent)

-- | Unit data type with a strict 'Semigroup' and 'Monoid' instances.
data StrictUnit = StrictUnit

-- | '<>' is strict, 'stimes' is /O(1)/
instance Semigroup StrictUnit where
  (<>)   = seq
  stimes = stimesIdempotent

-- | 'mappend' is strict
instance Monoid StrictUnit where
  mempty  = StrictUnit
  mappend = (<>)
