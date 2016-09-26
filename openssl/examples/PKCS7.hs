import Control.Monad
import Data.Time.Clock
import Data.Time.Calendar
import Data.Maybe
import OpenSSL
import OpenSSL.PKCS7
import OpenSSL.EVP.Cipher
import OpenSSL.EVP.PKey
import OpenSSL.PEM
import OpenSSL.RSA
import OpenSSL.X509
import OpenSSL.X509.Store

main = withOpenSSL $
       do rsa  <- generateRSAKey 512 65537 Nothing
          cert <- genCert rsa

          pkcs7 <- pkcs7Sign cert rsa [] "Hello, world!" [Pkcs7NoCerts]

          store <- newX509Store
          addCertToStore store cert

          pkcs7Verify pkcs7 [cert] store Nothing [] >>= print
          return ()


genCert :: KeyPair k => k -> IO X509
genCert pkey
    = do x509 <- newX509
         setVersion x509 2
         setSerialNumber x509 1
         setIssuerName  x509 [("C", "JP")]
         setSubjectName x509 [("C", "JP")]
         setNotBefore x509 =<< liftM (addUTCTime (-1)) getCurrentTime
         setNotAfter  x509 =<< liftM (addUTCTime (365 * 24 * 60 * 60)) getCurrentTime
         setPublicKey x509 pkey
         signX509 x509 pkey Nothing
         return x509
