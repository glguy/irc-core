{-|
Module      : Client.Commands.WordCompletion
Description : Tab-completion logic
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the tab-completion logic used for nicknames and channels.

-}
module Client.Commands.WordCompletion
  ( wordComplete
  ) where

import qualified Client.State.EditBox as Edit
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Text (Text)
import           Irc.Identifier

-- | Perform word completion on a text box.
--
-- The leading update operation is applied to the result of tab-completion
-- when tab completing from the beginning of the text box. This is useful
-- when auto-completing a nick and including a trailing colon.
--
-- The @reversed@ parameter indicates that tab-completion should return the
-- previous entry. When starting a fresh tab completion the priority completions
-- will be considered in order before resorting to the set of possible
-- completions.
wordComplete ::
  (String -> String) {- ^ leading update operation -} ->
  Bool               {- ^ reversed -} ->
  [Identifier]       {- ^ priority completions -} ->
  [Identifier]       {- ^ possible completions -} ->
  Edit.EditBox -> Maybe Edit.EditBox
wordComplete leadingCase isReversed hint vals box =
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
         do next <- find (idPrefix cur) hint <|>
                    tabSearch isReversed cur cur vals
            Just $ set Edit.tabSeed (Just current)
                 $ replaceWith leadingCase (idString next) box

replaceWith :: (String -> String) -> String -> Edit.EditBox -> Edit.EditBox
replaceWith leadingCase str box =
    let box1 = Edit.killWordBackward False box
        str1 | view Edit.pos box1 == 0 = leadingCase str
             | otherwise               = str
    in over Edit.content (Edit.insertString str1) box1

idString :: Identifier -> String
idString = Text.unpack . idText

currentWord :: Edit.EditBox -> String
currentWord box
  = reverse
  $ takeWhile (not . isSpace)
  $ dropWhile (\x -> x==' ' || x==':')
  $ reverse
  $ take n txt
 where Edit.Line n txt = view Edit.line box

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

