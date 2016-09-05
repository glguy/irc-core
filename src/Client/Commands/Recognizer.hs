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
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid
import Data.Text (Text, commonPrefixes, cons, uncons, null)
import Data.Maybe

import Prelude hiding (all,null,lookup)

data Recognizer a
  = Branch !Text !(Maybe a) !(HashMap Char (Recognizer a))
  deriving (Show, Functor)

instance Monoid (Recognizer a) where
  mempty = Branch "" Nothing empty
  mappend = both

data Recognition a
  = Exact a | Prefix [Text] | Invalid
  deriving (Show, Functor)

common :: Text -> Text -> (Text, Text, Text)
common l r = fromMaybe ("", l, r) $ commonPrefixes l r

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

single :: Text -> a -> Recognizer a
single tx v = Branch tx (Just $! v) empty

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

all :: [Recognizer a] -> Recognizer a
all [] = Branch "" Nothing empty
all [r] = r
all rs = all $ pair rs
 where
 pair (l:r:rest) = both l r : pair rest
 pair rest       = rest

fromCommands :: [(Text, a)] -> Recognizer a
fromCommands = all . map (uncurry single)

addCommand :: Text -> a -> Recognizer a -> Recognizer a
addCommand tx v = both $ single tx v

keys :: Recognizer a -> [Text]
keys (Branch pf contained children)
  = maybeToList (pf <$ contained)
  ++ (mappend pf <$> childKeys children)

childKeys :: HashMap Char (Recognizer a) -> [Text]
childKeys children = toList children >>= \(c,rec) -> cons c <$> keys rec
