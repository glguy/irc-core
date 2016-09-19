{-# Language OverloadedStrings #-}
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
import           Client.State
import           Control.Lens
import           Data.List
import qualified Data.Text as Text
import           Digraphs
import           Graphics.Vty.Image

-- | Render the lines of a table showing all of the available digraph entries
digraphLines ::
  ClientState {- ^ client state -} ->
  [Image]     {- ^ output lines -}
digraphLines st
  = map (horizCat . intersperse sep)
  $ chunksOf entriesPerLine
  $ map (text' defAttr)
  $ filter matcher
  $ map (Text.pack . drawEntry)
  $ digraphListToList digraphs
  where
    w              = view clientWidth st
    matcher        = clientMatcher st
    entriesPerLine = max 1 -- just in case?
                   $ (w + sepWidth) `quot` (entryWidth + sepWidth)

entryWidth :: Int
entryWidth = 5 -- "Ka カ"

sepWidth :: Int
sepWidth = imageWidth sep

sep :: Image
sep = text' defAttr "   "

drawEntry :: (Char,Char,Char) -> String
drawEntry (x,y,z) = output ++ replicate (entryWidth - wcswidth output) ' '
  where
    output = x:y:z2
    dottedCircle = '\x25cc'
    z1 = cleanChar z
    z2 | wcwidth z1 == 0 = [' ', dottedCircle, z1]
       | otherwise       = [' ', z1]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where
    (a,b) = splitAt n xs
