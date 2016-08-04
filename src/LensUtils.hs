{-|
Module      : LensUtils
Description : Lens utility functions
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides functions that are useful with lenses.

-}
module LensUtils
  ( Id'
  , overStrict
  , setStrict

  -- * time lenses
  , zonedTimeLocalTime
  , localTimeTimeOfDay
  ) where

import           Control.Lens
import           Control.Applicative
import           Data.Profunctor.Unsafe
import           Data.Time

newtype Id' a = Id' { runId' :: a }

-- | Strict function composition
instance Functor Id' where
  fmap = liftA

-- | Strict function application
instance Applicative Id' where
  pure = Id'
  Id' f <*> Id' x = Id' (f $! x)

-- | Modify the target of a 'Setter' with a function. The result
-- is strict in the results of applying the function. Strict version
-- of 'over'
overStrict :: LensLike Id' s t a b -> (a -> b) -> s -> t
overStrict l f = runId' #. l (Id' #. f)
{-# INLINE overStrict #-}

-- | Set a value strictly in the set value. Strict version of 'set'.
setStrict :: ASetter s t a b -> b -> s -> t
setStrict l x = set l $! x
{-# INLINE setStrict #-}

zonedTimeLocalTime :: Lens' ZonedTime LocalTime
zonedTimeLocalTime f (ZonedTime t z) = f t <&> \t' -> ZonedTime t' z

localTimeTimeOfDay :: Lens' LocalTime TimeOfDay
localTimeTimeOfDay f (LocalTime d t) = LocalTime d <$> f t
