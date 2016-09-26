module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Network.Socket
import OpenSSL
import OpenSSL.EVP.PKey
import OpenSSL.PEM
import OpenSSL.RSA
import qualified OpenSSL.Session as SSL
import Text.Printf

main = withOpenSSL (dumpPEM >> main')

dumpPEM = do pem      <- readFile "server.pem"
             Just key <- liftM toKeyPair $ readPrivateKey pem PwNone

             let n = rsaN key
                 e = rsaE key
                 d = rsaD key
             printf "n (public modulus) = %s\n" (show n)
             printf "e (public exponent) = %s\n" (show e)
             printf "d (private exponent) = %s\n" (show d)

main' = do
  sock <- socket AF_INET Stream 0
  bindSocket sock $ SockAddrInet (fromIntegral 4112) iNADDR_ANY
  setSocketOption sock ReuseAddr 1
  putStrLn "\n*** Listening to 4112/tcp ***"
  listen sock 1
  (sock', sockaddr) <- accept sock
  print $ "Accepted connection from " ++ show sockaddr

  ctx <- SSL.context
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv3
  SSL.contextSetPrivateKeyFile ctx "server.pem"
  SSL.contextSetCertificateFile ctx "server.crt"
  SSL.contextSetCiphers ctx "DEFAULT"
  SSL.contextCheckPrivateKey ctx >>= print
  conn <- SSL.connection ctx sock'
  SSL.accept conn
  b <- SSL.read conn 1024
  SSL.write conn b
  SSL.shutdown conn SSL.Bidirectional
