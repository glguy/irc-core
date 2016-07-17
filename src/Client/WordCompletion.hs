module Client.WordCompletion
  ( wordComplete
  ) where

import Irc.Identifier
import qualified Data.Text as Text
import qualified Data.ByteString as B
import qualified Data.Set as Set
import Data.Char
import Data.Function
import Data.List
import Control.Lens
import Client.EditBox as Edit
import Control.Monad

wordComplete :: Bool -> [Identifier] -> Edit.EditBox -> Maybe Edit.EditBox
wordComplete isReversed vals box =
  do let current = currentWord box
     guard (not (null current))
     let cur = mkId (Text.pack current)
     case view Edit.tabSeed box of
       Just pattern
         | let pat = mkId (Text.pack pattern)
         , idPrefix pat cur ->

         do next <- tabSearch isReversed pat cur vals
            Just $ replaceWith (idString next) box

       _ ->
         do next <- tabSearch isReversed cur cur vals
            Just $ set tabSeed (Just current)
                 $ replaceWith (idString next) box

replaceWith :: String -> Edit.EditBox -> Edit.EditBox
replaceWith str box =
    let box1 = Edit.killWord False box
        str1 | view Edit.pos box1 == 0 = str ++ ": "
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

tabSearch :: Bool -> Identifier -> Identifier -> [Identifier] -> Maybe Identifier
tabSearch isReversed pat cur vals
  | Just next <- advanceFun cur valSet
  , idPrefix pat next
  = Just next

  | isReversed = find (idPrefix pat) (reverse (Set.toList valSet))

  | otherwise  = do x <- Set.lookupGE pat valSet
                    guard (idPrefix pat x)
                    Just x
  where
    valSet = Set.fromList vals

    advanceFun | isReversed = Set.lookupLT
               | otherwise  = Set.lookupGT

