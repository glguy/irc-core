module LensUtils (overStrict, setStrict) where

import Control.Lens
import Control.Applicative
import Data.Profunctor.Unsafe

newtype Id' a = Id' { runId' :: a }

instance Functor Id' where
  fmap = liftA

instance Applicative Id' where
  pure = Id'
  Id' f <*> Id' x = Id' (f $! x)

overStrict :: LensLike Id' s t a b -> (a -> b) -> s -> t
overStrict l f = runId' #. l (Id' #. f)
{-# INLINE overStrict #-}

setStrict :: LensLike Id' s t a b -> b -> s -> t
setStrict l x = runId' #. l (\_ -> Id' x)
{-# INLINE setStrict #-}
