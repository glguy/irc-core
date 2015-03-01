{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Irc.List (List, length, fromList) where

import Prelude hiding (length, foldr)
import Control.Applicative
import Data.Foldable (Foldable(foldr))
import Data.Traversable (Traversable(traverse))
import Control.Lens (AsEmpty(..), Cons(..), prism')
import Data.Monoid

data List a
  = Cons !Int !a !(List a)
  | Nil
  deriving (Read, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons i x xs) = Cons i (f x) (fmap f xs)

instance Foldable List where
  foldr _ z Nil = z
  foldr f z (Cons _ x xs) = f x (foldr f z xs)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons i x xs) = Cons i <$> f x <*> traverse f xs

length :: List a -> Int
length Nil = 0
length (Cons n _ _) = n

fromList :: [a] -> List a
fromList = foldr cons Nil

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
