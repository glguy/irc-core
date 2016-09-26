{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import qualified Data.ByteString.Char8 as B8
import Data.List
import Data.Maybe
import Data.Monoid
import OpenSSL
import OpenSSL.EVP.Cipher
import OpenSSL.EVP.Open
import OpenSSL.EVP.PKey
import OpenSSL.EVP.Seal
import OpenSSL.PEM
import OpenSSL.RSA
import Text.Printf


main = withOpenSSL $
       do putStrLn "cipher: DES-CBC"
          des <- liftM fromJust $ getCipherByName "DES-CBC"

          putStrLn "generating RSA keypair..."
          rsa <- generateRSAKey 512 65537 Nothing

          let plainText = "Hello, world!"
          B8.putStrLn ("plain text to encrypt: " `mappend` plainText)

          putStrLn ""

          putStrLn "encrypting..."
          (encrypted, [encKey], iv) <- sealBS des [fromPublicKey rsa] plainText
          
          B8.putStrLn ("encrypted symmetric key: " `mappend` binToHex encKey)
          B8.putStrLn ("IV: " `mappend` binToHex iv)
          B8.putStrLn ("encrypted message: " `mappend` binToHex encrypted)

          putStrLn ""

          putStrLn "decrypting..."
          let decrypted = openBS des encKey iv rsa encrypted

          B8.putStrLn ("decrypted message: " `mappend` decrypted)


binToHex :: B8.ByteString -> B8.ByteString
binToHex = B8.pack . intercalate ":" . map (printf "%02x" . fromEnum) . B8.unpack
