{-# Language OverloadedStrings, DeriveFunctor #-}

{-|
Module      : Client.Commands.Recognizer
Description : Trie for recognizing commands
Copyright   : (c) Dan Doel, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module implements a trie for recognizing valid commands. This
allows entered strings to be classified as either a valid command
(with an associated value), the prefix of a valid command, or invalid.
-}

module Client.Commands.Recognizer
  ( Recognizer
  , recognize
  , Recognition(..)
  , fromCommands
  , addCommand
  , keys
  ) where

import Control.Monad
import Control.Applicative hiding (empty)

import Data.HashMap.Strict (lookup,insertWith,HashMap,empty,unionWith,fromList,toList)
import Data.Monoid
import Data.Text (Text, commonPrefixes, cons, uncons, null)
import Data.Maybe

import Prelude hiding (all,null,lookup)

-- | A map from 'Text' values to 'a' values that is capable of yielding more
-- detailed information when looking up keys that are not actually in the map.
data Recognizer a
  = Branch !Text !(Maybe a) !(HashMap Char (Recognizer a))
  deriving (Show, Functor)

instance Monoid (Recognizer a) where
  mempty = Branch "" Nothing empty
  mappend = both

-- | Possible results of recognizing text.
data Recognition a
  = Exact a       -- ^ text matched exactly, yielding the given value
  | Prefix [Text] -- ^ text would be recognized if joined to the given suffixes
  | Invalid       -- ^ text could not possibly be recognized
  deriving (Show, Functor)

-- | Match common prefixes of two strings in a more convenient form than
-- available from 'Data.Text'
common :: Text -> Text -> (Text, Text, Text)
common l r = fromMaybe ("", l, r) $ commonPrefixes l r

-- | Attempt to recognize a string, yielding a 'Recognition' result.
recognize :: Text -> Recognizer a -> Recognition a
recognize tx (Branch pf contained children)
  = case common pf tx of
      (common, pfsfx, txsfx) -> case uncons txsfx of
        Nothing
          | null pfsfx
          , Just a <- contained -> Exact a
          | otherwise -> Prefix $ keys (Branch pfsfx contained children)
        Just (c, txrest)
          | null pfsfx
          , Just rec <- lookup c children
          -> recognize txrest rec
        _ -> Invalid

-- | Create a singleton 'Recognizer' associating the given 'Text' and value.
single :: Text -> a -> Recognizer a
single tx v = Branch tx (Just $! v) empty

-- | Union two 'Recognizers'. The stored values in the result are biased to the
-- left if there is key overlap.
both :: Recognizer a -> Recognizer a -> Recognizer a
both (Branch pfl conl chil) (Branch pfr conr chir)
  = case common pfl pfr of
      (common, lsfx, rsfx) -> Branch common contained children
        where
        contained = (guard (null lsfx) *> conl) <|> (guard (null rsfx) *> conr)
        children = case (uncons lsfx, uncons rsfx) of
          (Nothing, Nothing)
            -> unionWith both chil chir
          (Just (l,lest), Nothing)
            -> insertWith (flip both) l (Branch lest conl chil) chir
          (Nothing, Just (r,rest))
            -> insertWith both r (Branch rest conr chir) chil
          (Just (l,lest), Just (r,rest))
            -> fromList [ (l, Branch lest conl chil)
                        , (r, Branch rest conr chir)
                        ]

-- | Union an arbitrary number of 'Recognizers' as with 'both'.
all :: [Recognizer a] -> Recognizer a
all [] = mempty
all [r] = r
all rs = all $ pair rs
 where
 pair (l:r:rest) = both l r : pair rest
 pair rest       = rest

-- | Create a 'Recognizer' from an association list. If a key appears twice, the
-- earliest associated value will be used.
fromCommands :: [(Text, a)] -> Recognizer a
fromCommands = all . map (uncurry single)

-- | Add a key-value pair to a 'Recognizer'. This will override the value
-- already present if one exists.
addCommand :: Text -> a -> Recognizer a -> Recognizer a
addCommand tx v = both $ single tx v

-- | Compute all strings that will be recognized by a 'Recognizer'.
keys :: Recognizer a -> [Text]
keys (Branch pf contained children)
  = maybeToList (pf <$ contained)
  ++ (mappend pf <$> childKeys children)

-- | Auxiliary function for 'keys'.
childKeys :: HashMap Char (Recognizer a) -> [Text]
childKeys children = toList children >>= \(c,rec) -> cons c <$> keys rec
