module Client.Image.MaskList where

import           Client.ChannelState
import           Client.ConnectionState
import           Client.NetworkConnection
import           Client.State
import           Control.Lens
import qualified Data.HashMap.Strict as HashMap
import           Data.List
import           Data.Ord
import           Data.Text (Text)
import           Data.Time
import           Graphics.Vty.Image
import           Irc.Identifier

maskListImages ::
  (Text -> Bool) -> Char -> NetworkName -> Identifier -> ClientState -> [Image]
maskListImages matcher mode network channel st
  | null entryList = [string (withForeColor defAttr red) "No masks"]
  | otherwise      = images
  where
    matcher' (x,(y,_)) = matcher x || matcher y

    entryList = sortBy (flip (comparing (snd . snd)))
              $ filter matcher'
              $ HashMap.toList entries

    entries = view ( clientConnection network
                   . csChannels . ix channel
                   . chanList mode
                   ) st

    renderWhen = formatTime defaultTimeLocale " %F %T"

    (masks, whoWhens) = unzip entryList
    maskImages       = text' defAttr <$> masks
    maskColumnWidth  = maximum (imageWidth <$> maskImages) + 1
    paddedMaskImages = resizeWidth maskColumnWidth <$> maskImages
    width            = max 1 (view clientWidth st)

    images = [ cropLine $ mask <|>
                          text' defAttr who <|>
                          string defAttr (renderWhen when)
             | (mask, (who, when)) <- zip paddedMaskImages whoWhens ]

    cropLine img
      | imageWidth img > width = cropRight width img
      | otherwise              = img
