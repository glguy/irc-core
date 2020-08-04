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
import           Control.Exception
import           Control.Applicative
import           Data.Maybe (fromMaybe)
import           Data.Time
import           Data.Text (Text)
import           Data.Foldable (foldl')
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as Text
import           Text.Read (readMaybe)
import           Text.Printf (printf)
import qualified OpenSSL.RSA as RSA
import qualified OpenSSL.X509 as X509
import qualified OpenSSL.PEM as PEM
import qualified OpenSSL.EVP.Digest as Digest

keysizeArg :: Args a (Maybe Int)
keysizeArg = optionalArg (tokenArg "[keysize]" (const parseSize))

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
      "Create new self-signed certificate at the given path with a given keysize.\n\
      \\n\
      \Key-size defaults to 2048 and must fall in the range 1024-8192.\n"
    (ClientCommand cmdNewCert noClientTab)

cmdNewCert :: ClientCommand (String, Maybe Int)
cmdNewCert st (path, mbSize) =
  do let size = fromMaybe 2048 mbSize
     rsa <- RSA.generateRSAKey' size 65537

     x509 <- X509.newX509
     X509.setVersion      x509 2
     X509.setSerialNumber x509 1
     X509.setIssuerName   x509 [("CN","glirc")]
     X509.setSubjectName  x509 [("CN","glirc")]
     X509.setNotBefore    x509 (UTCTime  (ModifiedJulianDay 40587) 0) -- 1970-01-01
     X509.setNotAfter     x509  (UTCTime (ModifiedJulianDay 77112) 0) -- 2070-01-01
     X509.setPublicKey    x509 rsa

     X509.signX509        x509 rsa Nothing

     der <- X509.writeDerX509 x509
     msgs1 <- getFingerprint der "sha1"
     msgs2 <- getFingerprint der "sha256"

     pem1 <- PEM.writePKCS8PrivateKey rsa Nothing
     pem2 <- PEM.writeX509 x509
     res <- try (writeFile path (pem1 ++ pem2))

     now <- getZonedTime
     case res of
       Left e ->
        commandFailure $ recordError now "" (Text.pack (displayException (e :: IOError))) st
       Right () ->
        commandSuccess $ foldl' (recordSuccess now) st (msgs1 ++ msgs2 ++ ["Certificate saved: " <> Text.pack path])

getFingerprint :: L.ByteString -> String -> IO [Text]
getFingerprint der name =
  do mb <- Digest.getDigestByName name
     pure $ case mb of
       Nothing -> []
       Just d  -> [Text.pack (name ++ " fingerprint: " ++ hexString (Digest.digestLBS d der))]

hexString :: B.ByteString -> String
hexString = B.foldr (printf "%02x%s") ""
