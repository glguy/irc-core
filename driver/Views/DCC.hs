module Views.DCC (dccImage) where

import qualified Data.Map as Map
import System.FilePath
import Data.Text.Encoding
import Control.Lens
import Graphics.Vty.Image

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
transferImg (Ongoing name total current _ _) =
    string defAttr name <|>
    string (withForeColor defAttr blue) (percent total current)
transferImg (Finished name _) =
    string defAttr name <|>
    char defAttr ' ' <|>
    string (withForeColor defAttr green) "100"

percent :: Int -> Int -> String
percent total current =
  let value = ((fromIntegral current) / (fromIntegral total)) * 100
  in ' ' : (take 5 . show $ value)
