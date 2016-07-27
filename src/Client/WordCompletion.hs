{-|
Module      : Client.WordCompletion
Description : Tab-completion logic
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the tab-completion logic used for nicknames and channels.

-}
module Client.WordCompletion
  ( wordComplete
  ) where

import Irc.Identifier
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString as B
import qualified Data.Set as Set
import Data.Char
import Data.Function
import Data.List
import Control.Lens
import Client.EditBox as Edit
import Control.Monad

-- | Perform word completion on a text box.
--
-- The leading update operation is applied to the result of tab-completion
-- when tab completing from the beginning of the text box. This is useful
-- when auto-completing a nick and including a trailing colon.
--
-- The @reversed@ parameter indicates that tab-completion should return the
-- previous entry.
wordComplete ::
  (String -> String) {- ^ leading update operation -} ->
  Bool               {- ^ reversed -} ->
  [Identifier]       {- ^ possible completions -} ->
  Edit.EditBox -> Maybe Edit.EditBox
wordComplete leadingCase isReversed vals box =
  do let current = currentWord box
     guard (not (null current))
     let cur = mkId (Text.pack current)
     case view Edit.tabSeed box of
       Just patternStr
         | idPrefix pat cur ->

         do next <- tabSearch isReversed pat cur vals
            Just $ replaceWith leadingCase (idString next) box
         where
           pat = mkId (Text.pack patternStr)

       _ ->
         do next <- tabSearch isReversed cur cur vals
            Just $ set tabSeed (Just current)
                 $ replaceWith leadingCase (idString next) box

replaceWith :: (String -> String) -> String -> Edit.EditBox -> Edit.EditBox
replaceWith leadingCase str box =
    let box1 = Edit.killWord False box
        str1 | view Edit.pos box1 == 0 = leadingCase str
             | otherwise               = str
    in Edit.insertString str1 box1

idPrefix :: Identifier -> Identifier -> Bool
idPrefix = B.isPrefixOf `on` idDenote

idString :: Identifier -> String
idString = Text.unpack . idText

currentWord :: Edit.EditBox -> String
currentWord box
  = reverse
  $ takeWhile (not . isSpace)
  $ dropWhile (\x -> x==' ' || x==':')
  $ reverse
  $ take (view Edit.pos box) (view Edit.content box)

class            Prefix a          where isPrefix :: a -> a -> Bool
instance         Prefix Identifier where isPrefix = idPrefix
instance         Prefix Text       where isPrefix = Text.isPrefixOf
instance Eq a => Prefix [a]        where isPrefix = isPrefixOf

tabSearch :: (Ord a, Prefix a) => Bool -> a -> a -> [a] -> Maybe a
tabSearch isReversed pat cur vals
  | Just next <- advanceFun cur valSet
  , isPrefix pat next
  = Just next

  | isReversed = find (isPrefix pat) (reverse (Set.toList valSet))

  | otherwise  = do x <- Set.lookupGE pat valSet
                    guard (isPrefix pat x)
                    Just x
  where
    valSet = Set.fromList vals

    advanceFun | isReversed = Set.lookupLT
               | otherwise  = Set.lookupGT

