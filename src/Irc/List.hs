{-# LANGUAGE FlexibleInstances     #-} -- instance Cons
{-# LANGUAGE MultiParamTypeClasses #-} -- instance Cons

-- | This module implements a strict list data type
-- which supports a constant time length function.
-- This is used for storing lists of messages in the
-- IRC connection state.
--
-- Most operations on this list are available via its
-- various instances.
module Irc.List
  ( List
  , length
  , fromList
  ) where

import Prelude hiding (length, foldr)
import Control.Applicative
import Data.Foldable (Foldable(foldr))
import Data.Traversable (Traversable(traverse))
import Control.Lens (AsEmpty(..), Cons(..), prism')
import Data.Monoid

-- | 'List' is strict in its elements and carries a
-- precomputed length.
data List a
  = Cons !Int !a !(List a)
  | Nil
  deriving (Read, Show)

instance Functor List where
  fmap _ Nil           = Nil
  fmap f (Cons i x xs) = Cons i (f x) (fmap f xs)

instance Foldable List where
  foldr _ z Nil           = z
  foldr f z (Cons _ x xs) = f x (foldr f z xs)

instance Traversable List where
  traverse _  Nil          = pure Nil
  traverse f (Cons i x xs) = Cons i <$> f x <*> traverse f xs

-- | Compute the length of a 'List' in constant time.
length :: List a -> Int
length Nil = 0
length (Cons n _ _) = n

-- | Construct a 'List' from a stanard list.
fromList :: [a] -> List a
fromList = foldr cons Nil

-- | Prepend an element onto the beginning of the list
cons :: a -> List a -> List a
cons x xs = Cons (length xs + 1) x xs

instance Cons (List a) (List a) a a where
  _Cons = prism' (uncurry cons) $ \xs ->
                case xs of
                  Nil -> Nothing
                  Cons _ y ys -> Just (y,ys)

instance AsEmpty (List a) where
  _Empty = prism' (\_ -> Nil) $ \xs ->
              case xs of
                Nil -> Just ()
                _   -> Nothing

instance Monoid (List a) where
  mempty = Nil
  mappend x y = foldr cons y x
