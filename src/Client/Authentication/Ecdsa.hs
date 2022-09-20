{-# Language OverloadedStrings #-}
{-|
Module      : Client.Authentication.Ecdsa
Description : Binding to ecdsatool
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

Implementation of ECDSA-NIST256P-CHALLENGE SASL authentication mode
as implemented at <https://github.com/kaniini/ecdsatool>.

Using this mode requires that the @ecdsa@ utility program is available
in your search path.

-}
module Client.Authentication.Ecdsa
  ( authenticationMode
  , encodeAuthentication
  , computeResponse
  ) where

import           Control.Exception (displayException, try)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           System.Process.Typed (readProcessStdout_, proc)
import           Irc.Commands (AuthenticatePayload(..))
import qualified Data.ByteString.Lazy as L


-- | Identifier for SASL ECDSA challenge response authentication
-- using curve NIST256P.
--
-- @ECDSA-NIST256P-CHALLENGE@
authenticationMode :: Text
authenticationMode = "ECDSA-NIST256P-CHALLENGE"


-- | Encode a username as specified in this authentication mode.
encodeAuthentication ::
  Maybe Text {- ^ authorization identity  -} ->
  Text {- ^ authentication identity -} ->
  AuthenticatePayload
encodeAuthentication Nothing authc =
  AuthenticatePayload (Text.encodeUtf8 authc)
encodeAuthentication (Just authz) authc =
  AuthenticatePayload (Text.encodeUtf8 (authc <> "\0" <> authz))


-- | Compute the response for a given challenge using the @ecdsatool@
-- executable which must be available in @PATH@.
computeResponse ::
  FilePath                {- ^ private key file                 -} ->
  Text                    {- ^ challenge string                 -} ->
  IO (Either String Text) {- ^ error message or response string -}
computeResponse privateKeyFile challenge =
  do res <- try (readProcessStdout_ (proc "ecdsatool" ["sign", privateKeyFile, Text.unpack challenge]))
     return $! case res of
                 Left e -> Left (displayException (e :: IOError))
                 Right resp ->
                     case Text.words <$> Text.decodeUtf8' (L.toStrict resp) of
                         Left e      -> Left (displayException e)
                         Right [str] -> Right str
                         Right _     -> Left "bad sasl ecdsa response message"
