{-|
Module      : Client.View.Digraphs
Description : Character mnemonics
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides an view of the built-in digraph list.

-}
module Client.View.Digraphs (digraphLines) where

import           Client.Image.Message (cleanChar)
import           Data.List
import           Digraphs
import           Graphics.Vty.Image

-- | Render the lines of a table showing all of the available digraph entries
digraphLines ::
  Int     {- ^ terminal width -} ->
  [Image] {- ^ output lines   -}
digraphLines w =
  [ horizCat (intersperse sep (map drawEntry xs))
     | xs <- chunksOf entriesPerLine digraphList ]
  where
    digraphList    = digraphListToList digraphs
    entriesPerLine = max 1 -- just in case?
                   $ (w + sepWidth) `quot` (entryWidth + sepWidth)

entryWidth :: Int
entryWidth = 5 -- "Ka カ"

sepWidth :: Int
sepWidth = imageWidth sep

sep :: Image
sep = string defAttr "   "

drawEntry :: (Char,Char,Char) -> Image
drawEntry (x,y,z) = resizeWidth entryWidth (string defAttr [x, y, ' ', cleanChar z])

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where
    (a,b) = splitAt n xs
