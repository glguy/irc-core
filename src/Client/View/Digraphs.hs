{-# Language OverloadedStrings #-}
{-|
Module      : Client.View.Digraphs
Description : Character mnemonics
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a view of the built-in digraph list.

-}
module Client.View.Digraphs (digraphLines) where

import           Client.Image.Message (cleanChar)
import           Client.Image.PackedImage
import           Client.State
import           Data.List
import           Data.List.Split
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import           Data.Text (Text)
import           Digraphs
import           Graphics.Vty.Attributes
import           Graphics.Vty.Image (wcwidth, wcswidth)

-- | Render the lines of a table showing all of the available digraph entries
digraphLines ::
  Int         {- ^ draw width   -} ->
  ClientState {- ^ client state -} ->
  [Image']    {- ^ output lines -}
digraphLines w st
  = map (mconcat . intersperse sep)
  $ chunksOf entriesPerLine
  $ map (text' defAttr)
  $ clientFilter st LText.fromStrict
  $ map (Text.pack . drawEntry)
  $ Text.chunksOf 3 digraphs
  where
    entriesPerLine = max 1 -- just in case?
                   $ (w + sepWidth) `quot` (entryWidth + sepWidth)

entryWidth :: Int
entryWidth = 5 -- "Ka カ"

sepWidth :: Int
sepWidth = imageWidth sep

sep :: Image'
sep = text' defAttr "   "

drawEntry :: Text {- ^ 3-character entry -} -> String
drawEntry entry = output ++ replicate (entryWidth - wcswidth output) ' '
  where
    [x,y,z] = Text.unpack entry
    output = x:y:z2
    dottedCircle = '\x25cc'
    z1 = cleanChar z
    z2 | wcwidth z1 == 0 = [' ', dottedCircle, z1]
       | otherwise       = [' ', z1]
