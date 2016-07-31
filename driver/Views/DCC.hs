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
  concat [ map transferImg . view clientDCCTransfers $ st
         , map offerImg . Map.assocs . view (clientServer0 . ccHoldDccTrans) $ st
         ]

offerImg :: (Identifier, DCCOffer) -> Image
offerImg (ident, offer) =
  string defAttr (takeFileName $ _doName offer)
  <|> char defAttr ' '
  <|> string (withForeColor defAttr red) (show $ _doSize offer `div` (1024^2))
  <|> string defAttr "Mb"
  <|> string defAttr " from "
  <|> text' defAttr (decodeUtf8 (idBytes ident))

transferImg :: Transfer -> Image
transferImg trans = string defAttr name <|>
                    pad (max 1 (70 - length name)) 0 0 0 (context trans)
  where
    name = _tName trans

    context (Finished _ _) = string (withForeColor defAttr green) "100%"
    context (Ongoing _ total current _ _) =
      string (withForeColor defAttr blue) (percent total current)

percent :: Int -> Int -> String
percent total current =
  let value :: Double
      value = ((fromIntegral current) / (fromIntegral total)) * 100
  in printf "%.1f%%" value
