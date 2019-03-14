{-# language LambdaCase #-}
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


-- TODO: maybe express more clearly?
dccImages :: ClientState -> [Image']
dccImages st =
  let dccState       = view clientDCC st
      (keys, offers) = views dsOffers (unzip . IntMap.toAscList) dccState
      pal            = clientPalette st

      imgKeys       = map (string (_palLabel pal) . show) keys
      imgNames      = map (string defAttr . _dccFileName) offers
      statusOff key = showStatus (statusAtKey key dccState)
      downloading   = map (string (_palMeta pal) . statusOff) keys
      percentage    = map (\k -> string (_palTextBox pal)
                            . maybe "  " (\p -> show p ++ "%") . fmap _dtProgress
                            $ view (dsTransfers . at k) dccState) keys

      downloadingNum = length . filter ((== Downloading) . flip statusAtKey dccState) $ keys

      countImage = string (view palLabel pal) "Offers (downloading/total): " <>
                   string defAttr (show downloadingNum) <>
                   char (view palLabel pal) '/' <>
                   string defAttr (show (length keys))
  in countImage :
       (reverse . createColumns
         $ transpose [imgKeys, imgNames, downloading, percentage])

createColumns :: [[Image']] -> [Image']
createColumns xs = map makeRow xs
  where
    columnWidths = maximum . map imageWidth <$> transpose xs
    makeRow = mconcat
            . intersperse (char defAttr ' ')
            . zipWith resizeImage columnWidths

showStatus :: ConnectionStatus -> String
showStatus = \case
  CorrectlyFinished -> "Finished"
  UserKilled        -> "Killed by user"
  LostConnection    -> "Socket failure"
  Downloading       -> "Downloading"
  Pending           -> "Pending"
  NotExist          -> "Not exist" -- Shouldn't be shown never
