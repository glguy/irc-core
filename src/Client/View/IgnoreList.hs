{-# Language OverloadedStrings #-}
{-|
Module      : Client.View.IgnoreList
Description : Line renderers for ignore mask list view
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module renders the lines used to list the ignore masks.
-}
module Client.View.IgnoreList
  ( ignoreListLines
  ) where

import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.Image.Message
import           Graphics.Vty.Attributes
import           Irc.Identifier
import           Data.HashSet (HashSet)
import           Data.Foldable
import           Data.Semigroup
import           Control.Lens

-- | Render the lines used in a channel mask list.
ignoreListLines ::
  HashSet Identifier {- ^ ignore masks -} ->
  Palette            {- ^ palette      -} ->
  [Image']
ignoreListLines ignores pal =
  summaryLine ignores pal :
  [ text' defAttr (cleanText (idText mask))
  | mask <- toList ignores ]


-- | Render a summary describing the number of ignore masks.
summaryLine ::
  HashSet Identifier {- ^ ignore masks -} ->
  Palette            {- ^ palette      -} ->
  Image'
summaryLine ignores pal
  | null ignores = text' (view palError pal) "Ignore list empty"
  | otherwise    = text' (view palLabel pal) "Ignore entries: "
                <> string defAttr (show (length ignores))
