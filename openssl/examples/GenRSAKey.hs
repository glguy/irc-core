import Control.Monad hiding (join)
import OpenSSL
import OpenSSL.EVP.PKey
import OpenSSL.PEM
import OpenSSL.RSA
import System.IO
import Text.Printf

main = withOpenSSL $
       do let keyBits = 512
              keyE    = 65537

          printf "Generating RSA key-pair, nbits = %d, e = %d:\n" keyBits keyE
          
          rsa  <- generateRSAKey keyBits keyE $ Just $ \ phase _ ->
                  do putChar $ case phase of
                                 0 -> '.'
                                 1 -> '+'
                                 2 -> '*'
                                 3 -> '\n'
                                 n -> head $ show n
                     hFlush stdout

          printf "Done.\n"
          
          let n    = rsaN rsa
              e    = rsaE rsa
              d    = rsaD rsa
              p    = rsaP rsa
              q    = rsaQ rsa

          printf "n (public modulus) = %s\n" (show n)
          printf "e (public exponent) = %s\n" (show e)
          printf "d (private exponent) = %s\n" (show d)
          printf "p (secret prime factor) = %s\n" (show p)
          printf "q (secret prime factor) = %s\n" (show q)

          writePKCS8PrivateKey rsa Nothing >>= putStr
          writePublicKey       rsa         >>= putStr
