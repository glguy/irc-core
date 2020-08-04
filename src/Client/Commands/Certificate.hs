{-# Language OverloadedStrings #-}
{-|
Module      : Client.Commands.Certificate
Description : Certificate management commands
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.Certificate (newCertificateCommand) where

import           Client.Commands.Arguments.Spec
import           Client.Commands.TabCompletion
import           Client.Commands.Types
import           Client.State
import           Control.Applicative
import           Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Foldable (foldl')
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time
import           Hookup.OpenSSL (getPubKeyDer)
import qualified OpenSSL.RSA as RSA
import qualified OpenSSL.X509 as X509
import qualified OpenSSL.PEM as PEM
import qualified OpenSSL.EVP.Cipher as Cipher
import qualified OpenSSL.EVP.Digest as Digest
import           Text.Read (readMaybe)
import           Text.Printf (printf)

keysizeArg :: Args a (Maybe (Int, String))
keysizeArg = optionalArg (liftA2 (,) (tokenArg "[keysize]" (const parseSize)) (remainingArg "[passphrase]"))

parseSize :: String -> Maybe Int
parseSize str =
  case readMaybe str of
    Just n | 1024 <= n, n <= 8192 -> Just n
    _ -> Nothing

newCertificateCommand :: Command
newCertificateCommand =
  Command
    (pure "new-self-signed-cert")
    (liftA2 (,) (simpleToken "filename") keysizeArg)
      "\^BParameters:\^B\n\
      \\n\
      \    filename:   Certificate and private key PEM output\n\
      \    keysize:    Public-key size (default 2048, range 1024-8192)\n\
      \    passphrase: Optional AES-128 private key passphrase\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Generate a new self-signed certificate for network service\n\
      \    identification.\n\
      \\n\
      \\^BExample command:\^B\n\
      \\n\
      \    /new-self-signed-cert /home/me/.glirc/config/my.pem 2048 SeCrEt\n\
      \\n\
      \\^BExample configuration:\^B\n\
      \    servers:\n\
      \      * name:                    \"fn\"\n\
      \        hostname:                \"chat.freenode.net\"\n\
      \        sasl: mechanism:         external\n"
      \        tls:                     yes\n\
      \        tls-client-cert:         \"my.pem\"\n\
      \        tls-client-key-password: \"SeCrEt\"\n\
    (ClientCommand cmdNewCert noClientTab)

cmdNewCert :: ClientCommand (String, Maybe (Int, String))
cmdNewCert st (path, mbExtra) =
  do now <- getZonedTime

     let size = case mbExtra of
                  Nothing -> 2048
                  Just (n,_) -> n
     pass <- case mbExtra of
               Just (_,p) | not (null p) ->
                 do cipher <- fromMaybe (error "No aes128!") <$> Cipher.getCipherByName "aes128"
                    pure (Just (cipher, PEM.PwStr p))
               _ -> pure Nothing

     rsa  <- RSA.generateRSAKey' size 65537
     x509 <- X509.newX509
     X509.setVersion      x509 2
     X509.setSerialNumber x509 1
     X509.setIssuerName   x509 [("CN","glirc")]
     X509.setSubjectName  x509 [("CN","glirc")]
     X509.setNotBefore    x509 (UTCTime (ModifiedJulianDay 40587) 0) -- 1970-01-01
     X509.setNotAfter     x509 (UTCTime (ModifiedJulianDay 77112) 0) -- 2070-01-01
     X509.setPublicKey    x509 rsa
     X509.signX509        x509 rsa Nothing

     ctder <- X509.writeDerX509 x509
     pkder <- getPubKeyDer x509
     msgss <- traverse (getFingerprint ctder pkder) ["sha1", "sha256", "sha512"]

     pem1 <- PEM.writePKCS8PrivateKey rsa pass
     pem2 <- PEM.writeX509 x509
     res  <- try (writeFile path (pem1 ++ pem2))

     case res of
       Left e ->
        commandFailure (recordError now "" (Text.pack (displayException (e :: IOError))) st)
       Right () ->
        do let msg = "Certificate saved: \x02" <> path <> "\x02"
           commandSuccess (foldl' (recordSuccess now) st (concat msgss ++ [Text.pack msg]))

getFingerprint :: L.ByteString -> B.ByteString -> String -> IO [Text]
getFingerprint crt pub name =
  do mb <- Digest.getDigestByName name
     pure $ case mb of
       Nothing -> []
       Just d  -> map Text.pack
         [printf "CERT %-6s fingerprint: \^C07%s" name (hexString (Digest.digestLBS d crt))
         ,printf "SPKI %-6s fingerprint: \^C07%s" name (hexString (Digest.digestBS  d pub))
         ]

hexString :: B.ByteString -> String
hexString = B.foldr (printf "%02x%s") ""
