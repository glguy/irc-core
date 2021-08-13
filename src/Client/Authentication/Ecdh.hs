{-# Language OverloadedStrings, ImportQualifiedPost #-}
module Client.Authentication.Ecdh where

import Crypto.Curve25519.Pure
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Irc.Commands (AuthenticatePayload (AuthenticatePayload))
import OpenSSL.EVP.Digest ( digestBS, getDigestByName, hmacBS, Digest )
import System.IO.Unsafe ( unsafePerformIO )

mechanismName :: Text
mechanismName = "ECDH-X25519-CHALLENGE"

clientFirst :: Maybe Text -> Text -> AuthenticatePayload
clientFirst Nothing authcid = AuthenticatePayload (Text.encodeUtf8 authcid)
clientFirst (Just authzid) authcid = AuthenticatePayload (Text.encodeUtf8 authcid <> "\0" <> Text.encodeUtf8 authzid)

computeResponse ::
  ByteString {- ^ server response -} ->
  Text       {- ^ private key -} ->
  Maybe AuthenticatePayload  {- ^ client response -}
computeResponse serverMessage privateKeyText = 
  do privateKey <- either (const Nothing) importPrivate
                 $ B64.decode (Text.encodeUtf8 privateKeyText)
     serverPublic <- importPublic serverPubBS
     
     let sharedSecret = makeShared privateKey serverPublic
     let ikm = digestBS sha2 (sharedSecret <> exportPublic (generatePublic privateKey) <> serverPubBS)
     let prk = hmacBS sha2 sessionSalt ikm
     let betterSecret = hmacBS sha2 prk "ECDH-X25519-CHALLENGE\1"
     let sessionChallenge = B.pack (B.zipWith xor maskedChallenge betterSecret)
     Just (AuthenticatePayload sessionChallenge)
  where
    (serverPubBS, (sessionSalt, maskedChallenge)) = B.splitAt 32 <$> B.splitAt 32 serverMessage

sha2 :: Digest
Just sha2 = unsafePerformIO (getDigestByName "SHA256")
