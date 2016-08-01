{-# LANGUAGE NoMonomorphismRestriction #-}
module Views.DCC  where

import qualified Data.Map as Map
import System.FilePath
import Data.Text.Encoding
import Control.Lens
import Graphics.Vty.Image
import Text.Printf

import ClientState
import DCC

import Irc.Format

dccImage :: ClientState -> [Image]
dccImage st =
  concat [ map transferImg . Map.assocs . view (focus . _2) $ st
         , map offerImg . Map.assocs . view (focus . _1) $ st
         ]
  where focus = clientServer0 . ccHoldDccTrans

offerImg :: (Identifier, DCCOffer) -> Image
offerImg (ident, offer) =
  string defAttr (takeFileName $ _doName offer)
  <|> char defAttr ' '
  <|> string (withForeColor defAttr red) (show $ _doSize offer `div` (1024^2))
  <|> string defAttr "Mb"
  <|> string defAttr " from "
  <|> text' defAttr (decodeUtf8 (idBytes ident))

transferImg :: (Identifier, Transfer) -> Image
transferImg (ident, trans) =
  string defAttr name
  <|> pad (max 1 (70 - length name)) 0 0 0 (context trans)
  <|> string defAttr " from "
  <|> text' defAttr (decodeUtf8 (idBytes ident))
  where
    name = _tName trans

    context (Finished _ _) = string (withForeColor defAttr green) "100%"
    context (Failed _ total current) =
      string (withForeColor defAttr red) (percent total current)
    context (Ongoing _ total current _ _) =
      string (withForeColor defAttr blue) (percent total current)

percent :: Int -> Int -> String
percent total current =
  let value :: Double
      value = ((fromIntegral current) / (fromIntegral total)) * 100
  in printf "%.1f%%" value
