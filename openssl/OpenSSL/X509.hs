{-# LANGUAGE CPP                      #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK prune             #-}
-- |An interface to X.509 certificate.
module OpenSSL.X509
    ( -- * Type
      X509
    , X509_

      -- * Functions to manipulate certificate
    , newX509
    , wrapX509 -- private
    , withX509Ptr -- private
    , withX509Stack -- private
    , unsafeX509ToPtr -- private
    , touchX509 -- private

    , compareX509

    , signX509
    , verifyX509

    , printX509

      -- * Accessors
    , getVersion
    , setVersion

    , getSerialNumber
    , setSerialNumber

    , getIssuerName
    , setIssuerName

    , getSubjectName
    , setSubjectName

    , getNotBefore
    , setNotBefore

    , getNotAfter
    , setNotAfter

    , getPublicKey
    , setPublicKey

    , getSubjectEmail
    )
    where
import Control.Monad
import Data.Time.Clock
import Data.Maybe
import Foreign.ForeignPtr
#if MIN_VERSION_base(4,4,0)
import Foreign.ForeignPtr.Unsafe as Unsafe
#else
import Foreign.ForeignPtr as Unsafe
#endif
import Foreign.Ptr
import Foreign.C
import OpenSSL.ASN1
import OpenSSL.BIO
import OpenSSL.EVP.Digest
import OpenSSL.EVP.PKey
import OpenSSL.EVP.Verify
import OpenSSL.EVP.Internal
import OpenSSL.Utils
import OpenSSL.Stack
import OpenSSL.X509.Name

-- |@'X509'@ is an opaque object that represents X.509 certificate.
newtype X509  = X509 (ForeignPtr X509_)
data    X509_


foreign import ccall unsafe "X509_new"
        _new :: IO (Ptr X509_)

foreign import ccall unsafe "&X509_free"
        _free :: FunPtr (Ptr X509_ -> IO ())

foreign import ccall unsafe "X509_print"
        _print :: Ptr BIO_ -> Ptr X509_ -> IO CInt

foreign import ccall unsafe "X509_cmp"
        _cmp :: Ptr X509_ -> Ptr X509_ -> IO CInt

foreign import ccall unsafe "HsOpenSSL_X509_get_version"
        _get_version :: Ptr X509_ -> IO CLong

foreign import ccall unsafe "X509_set_version"
        _set_version :: Ptr X509_ -> CLong -> IO CInt

foreign import ccall unsafe "X509_get_serialNumber"
        _get_serialNumber :: Ptr X509_ -> IO (Ptr ASN1_INTEGER)

foreign import ccall unsafe "X509_set_serialNumber"
        _set_serialNumber :: Ptr X509_ -> Ptr ASN1_INTEGER -> IO CInt

foreign import ccall unsafe "X509_get_issuer_name"
        _get_issuer_name :: Ptr X509_ -> IO (Ptr X509_NAME)

foreign import ccall unsafe "X509_set_issuer_name"
        _set_issuer_name :: Ptr X509_ -> Ptr X509_NAME -> IO CInt

foreign import ccall unsafe "X509_get_subject_name"
        _get_subject_name :: Ptr X509_ -> IO (Ptr X509_NAME)

foreign import ccall unsafe "X509_set_subject_name"
        _set_subject_name :: Ptr X509_ -> Ptr X509_NAME -> IO CInt

foreign import ccall unsafe "HsOpenSSL_X509_get_notBefore"
        _get_notBefore :: Ptr X509_ -> IO (Ptr ASN1_TIME)

foreign import ccall unsafe "X509_set_notBefore"
        _set_notBefore :: Ptr X509_ -> Ptr ASN1_TIME -> IO CInt

foreign import ccall unsafe "HsOpenSSL_X509_get_notAfter"
        _get_notAfter :: Ptr X509_ -> IO (Ptr ASN1_TIME)

foreign import ccall unsafe "X509_set_notAfter"
        _set_notAfter :: Ptr X509_ -> Ptr ASN1_TIME -> IO CInt

foreign import ccall unsafe "X509_get_pubkey"
        _get_pubkey :: Ptr X509_ -> IO (Ptr EVP_PKEY)

foreign import ccall unsafe "X509_set_pubkey"
        _set_pubkey :: Ptr X509_ -> Ptr EVP_PKEY -> IO CInt

foreign import ccall unsafe "X509_get1_email"
        _get1_email :: Ptr X509_ -> IO (Ptr STACK)

foreign import ccall unsafe "X509_email_free"
        _email_free :: Ptr STACK -> IO ()

foreign import ccall unsafe "X509_sign"
        _sign :: Ptr X509_ -> Ptr EVP_PKEY -> Ptr EVP_MD -> IO CInt

foreign import ccall unsafe "X509_verify"
        _verify :: Ptr X509_ -> Ptr EVP_PKEY -> IO CInt

-- |@'newX509'@ creates an empty certificate. You must set the
-- following properties to and sign it (see 'signX509') to actually
-- use the certificate.
--
--   [/Version/] See 'setVersion'.
--
--   [/Serial number/] See 'setSerialNumber'.
--
--   [/Issuer name/] See 'setIssuerName'.
--
--   [/Subject name/] See 'setSubjectName'.
--
--   [/Validity/] See 'setNotBefore' and 'setNotAfter'.
--
--   [/Public Key/] See 'setPublicKey'.
--
newX509 :: IO X509
newX509 = _new >>= failIfNull >>= wrapX509


wrapX509 :: Ptr X509_ -> IO X509
wrapX509 = fmap X509 . newForeignPtr _free


withX509Ptr :: X509 -> (Ptr X509_ -> IO a) -> IO a
withX509Ptr (X509 x509) = withForeignPtr x509


withX509Stack :: [X509] -> (Ptr STACK -> IO a) -> IO a
withX509Stack = withForeignStack unsafeX509ToPtr touchX509


unsafeX509ToPtr :: X509 -> Ptr X509_
unsafeX509ToPtr (X509 x509) = Unsafe.unsafeForeignPtrToPtr x509


touchX509 :: X509 -> IO ()
touchX509 (X509 x509) = touchForeignPtr x509

-- |@'compareX509' cert1 cert2@ compares two certificates.
compareX509 :: X509 -> X509 -> IO Ordering
compareX509 cert1 cert2
    = withX509Ptr cert1 $ \ cert1Ptr ->
      withX509Ptr cert2 $ \ cert2Ptr ->
      fmap interpret (_cmp cert1Ptr cert2Ptr)
    where
      interpret :: CInt -> Ordering
      interpret n
          | n > 0     = GT
          | n < 0     = LT
          | otherwise = EQ

-- |@'signX509'@ signs a certificate with an issuer private key.
signX509 :: KeyPair key =>
            X509         -- ^ The certificate to be signed.
         -> key          -- ^ The private key to sign with.
         -> Maybe Digest -- ^ A hashing algorithm to use. If @Nothing@
                         --   the most suitable algorithm for the key
                         --   is automatically used.
         -> IO ()
signX509 x509 key mDigest
    = withX509Ptr x509 $ \ x509Ptr ->
      withPKeyPtr' key $ \ pkeyPtr ->
      do dig <- case mDigest of
                  Just md -> return md
                  Nothing -> pkeyDefaultMD key
         withMDPtr dig $ \ digestPtr ->
             _sign x509Ptr pkeyPtr digestPtr
                  >>= failIf_ (== 0)
         return ()

-- |@'verifyX509'@ verifies a signature of certificate with an issuer
-- public key.
verifyX509 :: PublicKey key =>
              X509 -- ^ The certificate to be verified.
           -> key  -- ^ The public key to verify with.
           -> IO VerifyStatus
verifyX509 x509 key
    = withX509Ptr x509 $ \ x509Ptr ->
      withPKeyPtr' key $ \ pkeyPtr ->
      _verify x509Ptr pkeyPtr
           >>= interpret
    where
      interpret :: CInt -> IO VerifyStatus
      interpret 1 = return VerifySuccess
      interpret 0 = return VerifyFailure
      interpret _ = raiseOpenSSLError

-- |@'printX509' cert@ translates a certificate into human-readable
-- format.
printX509 :: X509 -> IO String
printX509 x509
    = do mem <- newMem
         withX509Ptr x509 $ \ x509Ptr ->
             withBioPtr mem $ \ memPtr ->
                 _print memPtr x509Ptr
                      >>= failIf_ (/= 1)
         bioRead mem

-- |@'getVersion' cert@ returns the version number of certificate. It
-- seems the number is 0-origin: version 2 means X.509 v3.
getVersion :: X509 -> IO Int
getVersion x509
    = withX509Ptr x509 $ \ x509Ptr ->
      liftM fromIntegral $ _get_version x509Ptr

-- |@'setVersion' cert ver@ updates the version number of certificate.
setVersion :: X509 -> Int -> IO ()
setVersion x509 ver
    = withX509Ptr x509 $ \ x509Ptr ->
      _set_version x509Ptr (fromIntegral ver)
           >>= failIf (/= 1)
           >>  return ()

-- |@'getSerialNumber' cert@ returns the serial number of certificate.
getSerialNumber :: X509 -> IO Integer
getSerialNumber x509
    = withX509Ptr x509 $ \ x509Ptr ->
      _get_serialNumber x509Ptr
           >>= peekASN1Integer

-- |@'setSerialNumber' cert num@ updates the serial number of
-- certificate.
setSerialNumber :: X509 -> Integer -> IO ()
setSerialNumber x509 serial
    = withX509Ptr x509 $ \ x509Ptr ->
      withASN1Integer serial $ \ serialPtr ->
      _set_serialNumber x509Ptr serialPtr
           >>= failIf (/= 1)
           >>  return ()

-- |@'getIssuerName'@ returns the issuer name of certificate.
getIssuerName :: X509 -- ^ The certificate to examine.
              -> Bool -- ^ @True@ if you want the keys of each parts
                      --   to be of long form (e.g. \"commonName\"),
                      --   or @False@ if you don't (e.g. \"CN\").
              -> IO [(String, String)] -- ^ Pairs of key and value,
                                       -- for example \[(\"C\",
                                       -- \"JP\"), (\"ST\",
                                       -- \"Some-State\"), ...\].
getIssuerName x509 wantLongName
    = withX509Ptr x509 $ \ x509Ptr ->
      do namePtr <- _get_issuer_name x509Ptr
         peekX509Name namePtr wantLongName

-- |@'setIssuerName' cert name@ updates the issuer name of
-- certificate. Keys of each parts may be of either long form or short
-- form. See 'getIssuerName'.
setIssuerName :: X509 -> [(String, String)] -> IO ()
setIssuerName x509 issuer
    = withX509Ptr x509 $ \ x509Ptr ->
      withX509Name issuer $ \ namePtr ->
      _set_issuer_name x509Ptr namePtr
           >>= failIf (/= 1)
           >>  return ()

-- |@'getSubjectName' cert wantLongName@ returns the subject name of
-- certificate. See 'getIssuerName'.
getSubjectName :: X509 -> Bool -> IO [(String, String)]
getSubjectName x509 wantLongName
    = withX509Ptr x509 $ \ x509Ptr ->
      do namePtr <- _get_subject_name x509Ptr
         peekX509Name namePtr wantLongName

-- |@'setSubjectName' cert name@ updates the subject name of
-- certificate. See 'setIssuerName'.
setSubjectName :: X509 -> [(String, String)] -> IO ()
setSubjectName x509 subject
    = withX509Ptr x509 $ \ x509Ptr ->
      withX509Name subject $ \ namePtr ->
      _set_subject_name x509Ptr namePtr
           >>= failIf (/= 1)
           >>  return ()

-- |@'getNotBefore' cert@ returns the time when the certificate begins
-- to be valid.
getNotBefore :: X509 -> IO UTCTime
getNotBefore x509
    = withX509Ptr x509 $ \ x509Ptr ->
      _get_notBefore x509Ptr
           >>= peekASN1Time

-- |@'setNotBefore' cert utc@ updates the time when the certificate
-- begins to be valid.
setNotBefore :: X509 -> UTCTime -> IO ()
setNotBefore x509 utc
    = withX509Ptr x509 $ \ x509Ptr ->
      withASN1Time utc $ \ time ->
      _set_notBefore x509Ptr time
           >>= failIf (/= 1)
           >>  return ()

-- |@'getNotAfter' cert@ returns the time when the certificate
-- expires.
getNotAfter :: X509 -> IO UTCTime
getNotAfter x509
    = withX509Ptr x509 $ \ x509Ptr ->
      _get_notAfter x509Ptr
           >>= peekASN1Time

-- |@'setNotAfter' cert utc@ updates the time when the certificate
-- expires.
setNotAfter :: X509 -> UTCTime -> IO ()
setNotAfter x509 utc
    = withX509Ptr x509 $ \ x509Ptr ->
      withASN1Time utc $ \ time ->
      _set_notAfter x509Ptr time
           >>= failIf (/= 1)
           >>  return ()

-- |@'getPublicKey' cert@ returns the public key of the subject of
-- certificate.
getPublicKey :: X509 -> IO SomePublicKey
getPublicKey x509
    = withX509Ptr x509 $ \ x509Ptr ->
      fmap fromJust ( _get_pubkey x509Ptr
                      >>= failIfNull
                      >>= wrapPKeyPtr
                      >>= fromPKey
                    )

-- |@'setPublicKey' cert pubkey@ updates the public key of the subject
-- of certificate.
setPublicKey :: PublicKey key => X509 -> key -> IO ()
setPublicKey x509 key
    = withX509Ptr x509 $ \ x509Ptr ->
      withPKeyPtr' key $ \ pkeyPtr ->
      _set_pubkey x509Ptr pkeyPtr
           >>= failIf (/= 1)
           >>  return ()

-- |@'getSubjectEmail' cert@ returns every subject email addresses in
-- the certificate.
getSubjectEmail :: X509 -> IO [String]
getSubjectEmail x509
    = withX509Ptr x509 $ \ x509Ptr ->
      do st   <- _get1_email x509Ptr
         list <- mapStack peekCString st
         _email_free st
         return list
