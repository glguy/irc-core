{-# Language BangPatterns #-}
{-|
Module      : Client.View.UrlSelection
Description : URL selection module
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a list of the URLs found in the current message
window in order to assist in selecting one to open with @/url@

-}
module Client.View.UrlSelection
  ( urlSelectionView
  ) where

import           Client.State
import           Client.State.Window
import           Client.State.Focus
import           Client.Image.Message
import           Control.Lens
import           Graphics.Vty.Image
import           Text.Regex.TDFA
import           Data.Text (Text)
import qualified Data.Text as Text


urlSelectionView :: Focus -> ClientState -> [Image]
urlSelectionView focus st =
    zipWith draw [1..]
         $ toListOf (clientWindows . ix focus . winMessages . folded . wlText . folding textUrls) st

textUrls :: Text -> [Text]
textUrls = getAllTextMatches . match urlPattern

draw :: Int -> Text -> Image
draw i url = string defAttr (shows i ". ")
         <|> text' defAttr (cleanText url)
