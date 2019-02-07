{-|
Module      : Client.View.DCCList
Description : View of the DCC offers and transfers
Copyright   : (c) Ruben Astudillo, 2019
License     : ISC
Maintainer  : ruben.astud@gmail.com

This module implements the rendering of the client DCC offer list.

-}
module Client.View.DCCList
  ( dccImages
  ) where

import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.State
import           Control.Lens
import           Data.List
import qualified Data.IntMap as IntMap
import           Graphics.Vty.Attributes
import           Client.State.DCC


dccImages :: ClientState -> [Image']
dccImages st =
  let (keys, offers) = unzip $ views clientDCCOffers IntMap.toAscList st
      pal = clientPalette st
      imgKeys = map (string (_palLabel pal) . show) keys
      imgNames = map (text' defAttr . view dccFileName) offers
  in createColumns $ transpose [imgKeys, imgNames]

createColumns :: [[Image']] -> [Image']
createColumns xs = map makeRow xs
  where
    columnWidths = maximum . map imageWidth <$> transpose xs
    makeRow = mconcat
            . intersperse (char defAttr ' ')
            . zipWith resizeImage columnWidths
