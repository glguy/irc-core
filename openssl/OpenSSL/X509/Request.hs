{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK prune             #-}
-- |An interface to PKCS#10 certificate request.
module OpenSSL.X509.Request
    ( -- * Type
      X509Req
    , X509_REQ -- private

      -- * Functions to manipulate request
    , newX509Req
    , wrapX509Req -- private
    , withX509ReqPtr -- private

    , signX509Req
    , verifyX509Req

    , printX509Req

    , makeX509FromReq

      -- * Accessors
    , getVersion
    , setVersion

    , getSubjectName
    , setSubjectName

    , getPublicKey
    , setPublicKey
    )
    where

import           Control.Monad
import           Data.Maybe
import           Foreign
import           Foreign.C
import           OpenSSL.BIO
import           OpenSSL.EVP.Digest hiding (digest)
import           OpenSSL.EVP.PKey
import           OpenSSL.EVP.Verify
import           OpenSSL.EVP.Internal
import           OpenSSL.Utils
import           OpenSSL.X509 (X509)
import qualified OpenSSL.X509 as Cert
import           OpenSSL.X509.Name

-- |@'X509Req'@ is an opaque object that represents PKCS#10
-- certificate request.
newtype X509Req  = X509Req (ForeignPtr X509_REQ)
data    X509_REQ


foreign import ccall unsafe "X509_REQ_new"
        _new :: IO (Ptr X509_REQ)

foreign import ccall unsafe "&X509_REQ_free"
        _free :: FunPtr (Ptr X509_REQ -> IO ())

foreign import ccall unsafe "X509_REQ_sign"
        _sign :: Ptr X509_REQ -> Ptr EVP_PKEY -> Ptr EVP_MD -> IO CInt

foreign import ccall unsafe "X509_REQ_verify"
        _verify :: Ptr X509_REQ -> Ptr EVP_PKEY -> IO CInt

foreign import ccall unsafe "X509_REQ_print"
        _print :: Ptr BIO_ -> Ptr X509_REQ -> IO CInt

foreign import ccall unsafe "HsOpenSSL_X509_REQ_get_version"
        _get_version :: Ptr X509_REQ -> IO CLong

foreign import ccall unsafe "X509_REQ_set_version"
        _set_version :: Ptr X509_REQ -> CLong -> IO CInt

foreign import ccall unsafe "HsOpenSSL_X509_REQ_get_subject_name"
        _get_subject_name :: Ptr X509_REQ -> IO (Ptr X509_NAME)

foreign import ccall unsafe "X509_REQ_set_subject_name"
        _set_subject_name :: Ptr X509_REQ -> Ptr X509_NAME -> IO CInt

foreign import ccall unsafe "X509_REQ_get_pubkey"
        _get_pubkey :: Ptr X509_REQ -> IO (Ptr EVP_PKEY)

foreign import ccall unsafe "X509_REQ_set_pubkey"
        _set_pubkey :: Ptr X509_REQ -> Ptr EVP_PKEY -> IO CInt

-- |@'newX509Req'@ creates an empty certificate request. You must set
-- the following properties to and sign it (see 'signX509Req') to
-- actually use the certificate request.
--
--  [/Version/] See 'setVersion'.
--
--  [/Subject Name/] See 'setSubjectName'.
--
--  [/Public Key/] See 'setPublicKey'.
--
newX509Req :: IO X509Req
newX509Req = _new >>= wrapX509Req


wrapX509Req :: Ptr X509_REQ -> IO X509Req
wrapX509Req = fmap X509Req . newForeignPtr _free


withX509ReqPtr :: X509Req -> (Ptr X509_REQ -> IO a) -> IO a
withX509ReqPtr (X509Req req) = withForeignPtr req

-- |@'signX509Req'@ signs a certificate request with a subject private
-- key.
signX509Req :: KeyPair key =>
               X509Req      -- ^ The request to be signed.
            -> key          -- ^ The private key to sign with.
            -> Maybe Digest -- ^ A hashing algorithm to use. If
                            --   @Nothing@ the most suitable algorithm
                            --   for the key is automatically used.
            -> IO ()
signX509Req req pkey mDigest
    = withX509ReqPtr req  $ \ reqPtr  ->
      withPKeyPtr'   pkey $ \ pkeyPtr ->
      do digest <- case mDigest of
                     Just md -> return md
                     Nothing -> pkeyDefaultMD pkey
         withMDPtr digest $ \ digestPtr ->
             _sign reqPtr pkeyPtr digestPtr
                  >>= failIf_ (== 0)

-- |@'verifyX509Req'@ verifies a signature of certificate request with
-- a subject public key.
verifyX509Req :: PublicKey key =>
                 X509Req -- ^ The request to be verified.
              -> key     -- ^ The public key to verify with.
              -> IO VerifyStatus
verifyX509Req req pkey
    = withX509ReqPtr req  $ \ reqPtr  ->
      withPKeyPtr'   pkey $ \ pkeyPtr ->
      _verify reqPtr pkeyPtr
           >>= interpret
    where
      interpret :: CInt -> IO VerifyStatus
      interpret 1 = return VerifySuccess
      interpret 0 = return VerifyFailure
      interpret _ = raiseOpenSSLError

-- |@'printX509Req' req@ translates a certificate request into
-- human-readable format.
printX509Req :: X509Req -> IO String
printX509Req req
    = do mem <- newMem
         withBioPtr mem $ \ memPtr ->
             withX509ReqPtr req $ \ reqPtr ->
                 _print memPtr reqPtr
                      >>= failIf_ (/= 1)
         bioRead mem

-- |@'getVersion' req@ returns the version number of certificate
-- request.
getVersion :: X509Req -> IO Int
getVersion req
    = withX509ReqPtr req $ \ reqPtr ->
      liftM fromIntegral $ _get_version reqPtr

-- |@'setVersion' req ver@ updates the version number of certificate
-- request.
setVersion :: X509Req -> Int -> IO ()
setVersion req ver
    = withX509ReqPtr req $ \ reqPtr ->
      _set_version reqPtr (fromIntegral ver)
           >>= failIf (/= 1)
           >>  return ()

-- |@'getSubjectName' req wantLongName@ returns the subject name of
-- certificate request. See 'OpenSSL.X509.getSubjectName' of
-- "OpenSSL.X509".
getSubjectName :: X509Req -> Bool -> IO [(String, String)]
getSubjectName req wantLongName
    = withX509ReqPtr req $ \ reqPtr ->
      do namePtr <- _get_subject_name reqPtr
         peekX509Name namePtr wantLongName

-- |@'setSubjectName' req name@ updates the subject name of
-- certificate request. See 'OpenSSL.X509.setSubjectName' of
-- "OpenSSL.X509".
setSubjectName :: X509Req -> [(String, String)] -> IO ()
setSubjectName req subject
    = withX509ReqPtr req $ \ reqPtr ->
      withX509Name subject $ \ namePtr ->
      _set_subject_name reqPtr namePtr
           >>= failIf (/= 1)
           >>  return ()

-- |@'getPublicKey' req@ returns the public key of the subject of
-- certificate request.
getPublicKey :: X509Req -> IO SomePublicKey
getPublicKey req
    = withX509ReqPtr req $ \ reqPtr ->
      fmap fromJust
           ( _get_pubkey reqPtr
             >>= failIfNull
             >>= wrapPKeyPtr
             >>= fromPKey
           )

-- |@'setPublicKey' req@ updates the public key of the subject of
-- certificate request.
setPublicKey :: PublicKey key => X509Req -> key -> IO ()
setPublicKey req pkey
    = withX509ReqPtr req  $ \ reqPtr  ->
      withPKeyPtr'   pkey $ \ pkeyPtr ->
      _set_pubkey reqPtr pkeyPtr
           >>= failIf (/= 1)
           >>  return ()


-- |@'makeX509FromReq' req cert@ creates an empty X.509 certificate
-- and copies as much data from the request as possible. The resulting
-- certificate doesn't have the following data and it isn't signed so
-- you must fill them and sign it yourself.
--
--   * Serial number
--
--   * Validity (Not Before and Not After)
--
-- Example:
--
-- > import Data.Time.Clock
-- >
-- > genCert :: X509 -> EvpPKey -> Integer -> Int -> X509Req -> IO X509
-- > genCert caCert caKey serial days req
-- >     = do cert <- makeX509FromReq req caCert
-- >          now  <- getCurrentTime
-- >          setSerialNumber cert serial
-- >          setNotBefore cert $ addUTCTime (-1) now
-- >          setNotAfter  cert $ addUTCTime (days * 24 * 60 * 60) now
-- >          signX509 cert caKey Nothing
-- >          return cert
--
makeX509FromReq :: X509Req
                -> X509
                -> IO X509
makeX509FromReq req caCert
    = do reqPubKey <- getPublicKey req
         verified  <- verifyX509Req req reqPubKey

         when (verified == VerifyFailure)
                  $ fail "makeX509FromReq: the request isn't properly signed by its own key."

         cert <- Cert.newX509
         Cert.setVersion cert 2 -- Version 2 means X509 v3. It's confusing.
         Cert.setIssuerName  cert =<< Cert.getSubjectName caCert False
         Cert.setSubjectName cert =<< getSubjectName req False
         Cert.setPublicKey   cert =<< getPublicKey req

         return cert
