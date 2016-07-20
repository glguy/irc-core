module Client.WordCompletion
  ( wordComplete
  , tabSearch
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

wordComplete :: (String -> String) -> Bool -> [Identifier] -> Edit.EditBox -> Maybe Edit.EditBox
wordComplete leadingCase isReversed vals box =
  do let current = currentWord box
     guard (not (null current))
     let cur = mkId (Text.pack current)
     case view Edit.tabSeed box of
       Just pattern
         | let pat = mkId (Text.pack pattern)
         , idPrefix pat cur ->

         do next <- tabSearch isReversed pat cur vals
            Just $ replaceWith leadingCase (idString next) box

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

