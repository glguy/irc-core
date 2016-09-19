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
import           Control.Exception (displayException, try)
import           Data.ByteArray.Encoding
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           System.IO.Error (IOError)
import           System.Process (readProcess)

authenticationMode :: Text
authenticationMode = "ECDSA-NIST256P-CHALLENGE"

encodeUsername :: Text -> Text
encodeUsername = Text.decodeUtf8 . convertToBase Base64 . Text.encodeUtf8

computeResponse :: FilePath -> Text -> IO (Either String Text)
computeResponse privateKeyFile challenge =
  do path <- resolveConfigurationPath privateKeyFile
     res  <- try (readProcess "ecdsatool" ["sign", path, Text.unpack challenge] "")
     return $! case words <$> res of
                 Right [resp] -> Right $! Text.pack resp
                 Right _      -> Left "bad sasl ecdsa response message"
                 Left e       -> Left (displayException (e :: IOError))
