module Views.DCC where

import ClientState
import Control.Lens
import Data.ByteString (ByteString)
import Data.List (partition)
import Data.Map (Map)
import Data.Monoid
import Graphics.Vty.Image
import ImageUtils
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map

import Irc.Format
import Irc.Model
import DCC

dccImage :: ClientState -> [Image]
dccImage = map progressImage . view clientDCCTransfers

progressImage :: Transfer -> Image
progressImage (Ongoing name total current _ _) =
    string defAttr name <|>
    string (withForeColor defAttr blue) (percent total current)
progressImage (Finished name size) =
    string defAttr name <|>
    string (withForeColor defAttr green) " 100"

percent :: Int -> Int -> String
percent total current =
  let value = ((fromIntegral current) / (fromIntegral total)) * 100
  in ' ' : (take 5 . show $ value)
