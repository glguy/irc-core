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
      statusOff key = case view (clientDCCTransfers . at key) st of
                        Nothing -> "Pending"
                        Just trans -> case view dtThread trans of
                                        Nothing -> "Finished"
                                        Just _ -> "Downloading"
      downloading = map (string (_palMeta pal) . statusOff) keys
      percentage = map (\k -> string (_palTextBox pal)
                         . maybe "  " (\p -> show p ++ "%") . fmap _dtProgress
                         $ view (clientDCCTransfers . at k) st) keys
  in reverse . createColumns
       $ transpose [imgKeys, imgNames, downloading, percentage]

createColumns :: [[Image']] -> [Image']
createColumns xs = map makeRow xs
  where
    columnWidths = maximum . map imageWidth <$> transpose xs
    makeRow = mconcat
            . intersperse (char defAttr ' ')
            . zipWith resizeImage columnWidths
