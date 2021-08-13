{-# Language OverloadedStrings, ImportQualifiedPost #-}
module Client.Authentication.Ecdh
  (
    -- * Phase type
    Phase1,
    -- * Mechanism details
    mechanismName,
    -- * Transition functions
    clientFirst,
    clientResponse,
  ) where

import Control.Monad (guard)
import Crypto.Curve25519.Pure qualified as Curve
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Irc.Commands (AuthenticatePayload (AuthenticatePayload))
import OpenSSL.EVP.Digest ( digestBS, getDigestByName, hmacBS, Digest )
import System.IO.Unsafe ( unsafePerformIO )

newtype Phase1 = Phase1 Curve.PrivateKey

mechanismName :: Text
mechanismName = "ECDH-X25519-CHALLENGE"

clientFirst :: Maybe Text -> Text -> Text -> Maybe (AuthenticatePayload, Phase1)
clientFirst mbAuthz authc privateKeyText =
  case Curve.importPrivate <$> B64.decode (Text.encodeUtf8 privateKeyText) of
    Right (Just private) -> Just (AuthenticatePayload payload, Phase1 private)
    _ -> Nothing
  where
    payload =
      case mbAuthz of
        Nothing    -> Text.encodeUtf8 authc
        Just authz -> Text.encodeUtf8 authc <> "\0" <> Text.encodeUtf8 authz

clientResponse ::
  Phase1 ->
  ByteString                {- ^ server response  -} ->
  Maybe AuthenticatePayload {- ^ client response  -}
clientResponse (Phase1 privateKey) serverMessage = 
  do let (serverPubBS, (sessionSalt, maskedChallenge)) =
           B.splitAt 32 <$> B.splitAt 32 serverMessage
     guard (B.length maskedChallenge == 32)
     serverPublic <- Curve.importPublic serverPubBS

     let sharedSecret = Curve.makeShared privateKey serverPublic
     let clientPublic = Curve.generatePublic privateKey
     let ikm = digestBS sha256
             $ sharedSecret <> Curve.exportPublic clientPublic <> serverPubBS
     let prk = hmacBS sha256 sessionSalt ikm
     let betterSecret = hmacBS sha256 prk "ECDH-X25519-CHALLENGE\1"
     let sessionChallenge = B.pack (B.zipWith xor maskedChallenge betterSecret)
     Just $! AuthenticatePayload sessionChallenge

sha256 :: Digest
Just sha256 = unsafePerformIO (getDigestByName "SHA256")
