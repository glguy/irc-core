{-# Language OverloadedStrings #-}
{-|
Module      : Client.Authentication.Ecdsa
Description : Binding to ecdsatool
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

Implementation of ECDSA-NIST256P-CHALLENGE SASL authentication mode
as implemented at <https://github.com/kaniini/ecdsatool> and used
on Freenode.

Using this mode requires that the @ecdsa@ utility program is available
in your search path.

-}
module Client.Authentication.Ecdsa
  ( authenticationMode
  , encodeUsername
  , computeResponse
  ) where

import           Client.Configuration (resolveConfigurationPath)
import           Control.Exception (try)
import           Data.ByteArray.Encoding
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           System.IO.Error (IOError)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process (readProcess)

authenticationMode :: Text
authenticationMode = "ECDSA-NIST256P-CHALLENGE"

encodeUsername :: Text -> Text
encodeUsername = Text.decodeUtf8 . convertToBase Base64 . Text.encodeUtf8

computeResponse :: FilePath -> Text -> Maybe Text
computeResponse privateKeyFile challenge =
  unsafePerformIO $
  do path <- resolveConfigurationPath privateKeyFile
     res <- try (readProcess "ecdsatool" ["sign", path, Text.unpack challenge] "")
     return $! case words <$> res :: Either IOError [String] of
                 Right [resp] -> Just $! Text.pack resp
                 _            -> Nothing
