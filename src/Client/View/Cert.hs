{-# Language OverloadedStrings #-}

{-|
Module      : Client.View.Cert
Description : Network certificate renderer
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Client.View.Cert
  ( certViewLines
  ) where

import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.Image.MircFormatting
import           Client.State
import           Client.State.Focus
import           Client.State.Network
import           Control.Lens
import           Data.Text (Text)
import qualified Data.Text.Lazy as LText

-- | Render the lines used in a channel mask list
certViewLines ::
  ClientState -> [Image']
certViewLines st
  | Just network <- currentNetwork st
  , Just cs <- preview (clientConnection network) st
  , let xs = view csCertificate cs
  , not (null xs)
  = map parseIrcText
  $ clientFilter st LText.fromStrict xs

  | otherwise = [text' (view palError pal) "No certificate available"]
  where
    pal = clientPalette st

currentNetwork :: ClientState -> Maybe Text
currentNetwork st =
  case view clientFocus st of
    NetworkFocus net   -> Just net
    ChannelFocus net _ -> Just net
    Unfocused          -> Nothing
