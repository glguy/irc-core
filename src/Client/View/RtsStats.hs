{-# Language CPP, OverloadedStrings #-}
{-|
Module      : Client.View.RtsStats
Description : View current GHC RTS statistics
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Lines for the @/rtsstats@ command. This module depends
on GHC 8.2.1 API.

-}

module Client.View.RtsStats
  ( rtsStatsLines
  ) where

import           Client.Image.PackedImage
import           Client.Image.Palette
import           Control.Lens
import           Graphics.Vty.Attributes
import           RtsStats

-- | Generate lines used for @/rtsstats@.
rtsStatsLines :: Maybe Stats -> Palette -> [Image']
rtsStatsLines Nothing pal = [text' (view palError pal) "Statistics not available"]
rtsStatsLines (Just stats) pal
  | null entries = [text' (view palError pal) "Statistics empty"]
  | otherwise    = zipWith (\v l -> padV wv v <> " " <> l) valueImages labelImages 
  where
    entries     = statsToEntries stats
    labelImages = map (text' (view palLabel pal) . fst) entries
    valueImages = map (text' defAttr . snd) entries
    wv          = maximum (0 : map imageWidth valueImages)
    padV n img  = string defAttr (replicate (n - imageWidth img) ' ') <> img
