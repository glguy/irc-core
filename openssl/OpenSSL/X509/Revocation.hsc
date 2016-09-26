{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK prune             #-}
-- |An interface to Certificate Revocation List.
module OpenSSL.X509.Revocation
    ( -- * Types
      CRL
    , X509_CRL -- privae
    , RevokedCertificate(..)

      -- * Functions to manipulate revocation list
    , newCRL
    , wrapCRL -- private
    , withCRLPtr -- private

    , signCRL
    , verifyCRL

    , printCRL

    , sortCRL

      -- * Accessors
    , getVersion
    , setVersion

    , getLastUpdate
    , setLastUpdate

    , getNextUpdate
    , setNextUpdate

    , getIssuerName
    , setIssuerName

    , getRevokedList
    , addRevoked
    , getRevoked
    )
    where
#include "HsOpenSSL.h"
import Control.Monad
#if OPENSSL_VERSION_NUMBER < 0x10000000
import Data.List
#endif
import Data.Time.Clock
import Data.Typeable
import Foreign
import Foreign.C
import OpenSSL.ASN1
import OpenSSL.BIO
import OpenSSL.EVP.Digest hiding (digest)
import OpenSSL.EVP.PKey
import OpenSSL.EVP.Verify
import OpenSSL.EVP.Internal
import OpenSSL.Stack
import OpenSSL.Utils
import OpenSSL.X509.Name

-- |@'CRL'@ is an opaque object that represents Certificate Revocation
-- List.
newtype CRL          = CRL (ForeignPtr X509_CRL)
data    X509_CRL
data    X509_REVOKED

-- |@'RevokedCertificate'@ represents a revoked certificate in a
-- list. Each certificates are supposed to be distinguishable by
-- issuer name and serial number, so it is sufficient to have only
-- serial number on each entries.
data RevokedCertificate
    = RevokedCertificate {
        revSerialNumber   :: Integer
      , revRevocationDate :: UTCTime
      }
    deriving (Show, Eq, Typeable)


foreign import ccall unsafe "X509_CRL_new"
        _new :: IO (Ptr X509_CRL)

foreign import ccall unsafe "&X509_CRL_free"
        _free :: FunPtr (Ptr X509_CRL -> IO ())

foreign import ccall unsafe "X509_CRL_sign"
        _sign :: Ptr X509_CRL -> Ptr EVP_PKEY -> Ptr EVP_MD -> IO CInt

foreign import ccall unsafe "X509_CRL_verify"
        _verify :: Ptr X509_CRL -> Ptr EVP_PKEY -> IO CInt

foreign import ccall unsafe "X509_CRL_print"
        _print :: Ptr BIO_ -> Ptr X509_CRL -> IO CInt

foreign import ccall unsafe "HsOpenSSL_X509_CRL_get_version"
        _get_version :: Ptr X509_CRL -> IO CLong

foreign import ccall unsafe "X509_CRL_set_version"
        _set_version :: Ptr X509_CRL -> CLong -> IO CInt

foreign import ccall unsafe "HsOpenSSL_X509_CRL_get_lastUpdate"
        _get_lastUpdate :: Ptr X509_CRL -> IO (Ptr ASN1_TIME)

foreign import ccall unsafe "X509_CRL_set_lastUpdate"
        _set_lastUpdate :: Ptr X509_CRL -> Ptr ASN1_TIME -> IO CInt

foreign import ccall unsafe "HsOpenSSL_X509_CRL_get_nextUpdate"
        _get_nextUpdate :: Ptr X509_CRL -> IO (Ptr ASN1_TIME)

foreign import ccall unsafe "X509_CRL_set_nextUpdate"
        _set_nextUpdate :: Ptr X509_CRL -> Ptr ASN1_TIME -> IO CInt

foreign import ccall unsafe "HsOpenSSL_X509_CRL_get_issuer"
        _get_issuer_name :: Ptr X509_CRL -> IO (Ptr X509_NAME)

foreign import ccall unsafe "X509_CRL_set_issuer_name"
        _set_issuer_name :: Ptr X509_CRL -> Ptr X509_NAME -> IO CInt

foreign import ccall unsafe "HsOpenSSL_X509_CRL_get_REVOKED"
        _get_REVOKED :: Ptr X509_CRL -> IO (Ptr STACK)

foreign import ccall unsafe "X509_CRL_add0_revoked"
        _add0_revoked :: Ptr X509_CRL -> Ptr X509_REVOKED -> IO CInt

#if OPENSSL_VERSION_NUMBER >= 0x10000000
-- This function is only available on OpenSSL 1.0.0 or later.
foreign import ccall unsafe "X509_CRL_get0_by_serial"
        _get0_by_serial :: Ptr X509_CRL -> Ptr (Ptr X509_REVOKED)
                        -> Ptr ASN1_INTEGER -> IO CInt
#endif

foreign import ccall unsafe "X509_CRL_sort"
        _sort :: Ptr X509_CRL -> IO CInt



foreign import ccall unsafe "X509_REVOKED_new"
        _new_revoked :: IO (Ptr X509_REVOKED)

foreign import ccall unsafe "X509_REVOKED_free"
        freeRevoked :: Ptr X509_REVOKED -> IO ()

foreign import ccall unsafe "X509_REVOKED_set_serialNumber"
        _set_serialNumber :: Ptr X509_REVOKED -> Ptr ASN1_INTEGER -> IO CInt

foreign import ccall unsafe "X509_REVOKED_set_revocationDate"
        _set_revocationDate :: Ptr X509_REVOKED -> Ptr ASN1_TIME -> IO CInt

-- |@'newCRL'@ creates an empty revocation list. You must set the
-- following properties to and sign it (see 'signCRL') to actually use
-- the revocation list. If you have any certificates to be listed, you
-- must of course add them (see 'addRevoked') before signing the list.
--
--   [/Version/] See 'setVersion'.
--
--   [/Last Update/] See 'setLastUpdate'.
--
--   [/Next Update/] See 'setNextUpdate'.
--
--   [/Issuer Name/] See 'setIssuerName'.
--
newCRL :: IO CRL
newCRL = _new >>= wrapCRL


wrapCRL :: Ptr X509_CRL -> IO CRL
wrapCRL = fmap CRL . newForeignPtr _free


withCRLPtr :: CRL -> (Ptr X509_CRL -> IO a) -> IO a
withCRLPtr (CRL crl) = withForeignPtr crl

-- |@'signCRL'@ signs a revocation list with an issuer private key.
signCRL :: KeyPair key =>
           CRL          -- ^ The revocation list to be signed.
        -> key          -- ^ The private key to sign with.
        -> Maybe Digest -- ^ A hashing algorithm to use. If @Nothing@
                        --   the most suitable algorithm for the key
                        --   is automatically used.
        -> IO ()
signCRL crl key mDigest
    = withCRLPtr crl   $ \ crlPtr  ->
      withPKeyPtr' key $ \ pkeyPtr ->
      do digest <- case mDigest of
                     Just md -> return md
                     Nothing -> pkeyDefaultMD key
         withMDPtr digest $ \ digestPtr ->
             _sign crlPtr pkeyPtr digestPtr
                  >>= failIf_ (== 0)
         return ()

-- |@'verifyCRL'@ verifies a signature of revocation list with an
-- issuer public key.
verifyCRL :: PublicKey key => CRL -> key -> IO VerifyStatus
verifyCRL crl key
    = withCRLPtr crl   $ \ crlPtr ->
      withPKeyPtr' key $ \ pkeyPtr ->
      _verify crlPtr pkeyPtr
           >>= interpret
    where
      interpret :: CInt -> IO VerifyStatus
      interpret 1 = return VerifySuccess
      interpret 0 = return VerifyFailure
      interpret _ = raiseOpenSSLError

-- |@'printCRL'@ translates a revocation list into human-readable
-- format.
printCRL :: CRL -> IO String
printCRL crl
    = do mem <- newMem
         withBioPtr mem $ \ memPtr ->
             withCRLPtr crl $ \ crlPtr ->
                 _print memPtr crlPtr
                      >>= failIf_ (/= 1)
         bioRead mem

-- |@'getVersion' crl@ returns the version number of revocation list.
getVersion :: CRL -> IO Int
getVersion crl
    = withCRLPtr crl $ \ crlPtr ->
      liftM fromIntegral $ _get_version crlPtr

-- |@'setVersion' crl ver@ updates the version number of revocation
-- list.
setVersion :: CRL -> Int -> IO ()
setVersion crl ver
    = withCRLPtr crl $ \ crlPtr ->
      _set_version crlPtr (fromIntegral ver)
           >>= failIf (/= 1)
           >>  return ()

-- |@'getLastUpdate' crl@ returns the time when the revocation list
-- has last been updated.
getLastUpdate :: CRL -> IO UTCTime
getLastUpdate crl
    = withCRLPtr crl $ \ crlPtr ->
      _get_lastUpdate crlPtr
           >>= peekASN1Time

-- |@'setLastUpdate' crl utc@ updates the time when the revocation
-- list has last been updated.
setLastUpdate :: CRL -> UTCTime -> IO ()
setLastUpdate crl utc
    = withCRLPtr crl $ \ crlPtr ->
      withASN1Time utc $ \ time ->
      _set_lastUpdate crlPtr time
           >>= failIf (/= 1)
           >>  return ()

-- |@'getNextUpdate' crl@ returns the time when the revocation list
-- will next be updated.
getNextUpdate :: CRL -> IO UTCTime
getNextUpdate crl
    = withCRLPtr crl $ \ crlPtr ->
      _get_nextUpdate crlPtr
           >>= peekASN1Time

-- |@'setNextUpdate' crl utc@ updates the time when the revocation
-- list will next be updated.
setNextUpdate :: CRL -> UTCTime -> IO ()
setNextUpdate crl utc
    = withCRLPtr crl $ \ crlPtr ->
      withASN1Time utc $ \ time ->
      _set_nextUpdate crlPtr time
           >>= failIf (/= 1)
           >>  return ()

-- |@'getIssuerName' crl wantLongName@ returns the issuer name of
-- revocation list. See 'OpenSSL.X509.getIssuerName' of
-- "OpenSSL.X509".
getIssuerName :: CRL -> Bool -> IO [(String, String)]
getIssuerName crl wantLongName
    = withCRLPtr crl $ \ crlPtr ->
      do namePtr <- _get_issuer_name crlPtr
         peekX509Name namePtr wantLongName

-- |@'setIssuerName' crl name@ updates the issuer name of revocation
-- list. See 'OpenSSL.X509.setIssuerName' of "OpenSSL.X509".
setIssuerName :: CRL -> [(String, String)] -> IO ()
setIssuerName crl issuer
    = withCRLPtr crl  $ \ crlPtr  ->
      withX509Name issuer $ \ namePtr ->
      _set_issuer_name crlPtr namePtr
           >>= failIf (/= 1)
           >>  return ()

-- |@'getRevokedList' crl@ returns the list of revoked certificates.
getRevokedList :: CRL -> IO [RevokedCertificate]
getRevokedList crl
    = withCRLPtr crl $ \ crlPtr ->
        _get_REVOKED crlPtr >>= mapStack peekRevoked

peekRevoked :: Ptr X509_REVOKED -> IO RevokedCertificate
peekRevoked rev = do
  serial <- peekASN1Integer =<< (#peek X509_REVOKED, serialNumber  ) rev
  date   <- peekASN1Time    =<< (#peek X509_REVOKED, revocationDate) rev
  return RevokedCertificate { revSerialNumber   = serial
                            , revRevocationDate = date
                            }

newRevoked :: RevokedCertificate -> IO (Ptr X509_REVOKED)
newRevoked revoked
    = do revPtr  <- _new_revoked

         seriRet <- withASN1Integer (revSerialNumber revoked) $
                    _set_serialNumber revPtr

         dateRet <- withASN1Time (revRevocationDate revoked) $
                    _set_revocationDate revPtr

         if seriRet /= 1 || dateRet /= 1 then
             freeRevoked revPtr >> raiseOpenSSLError
           else
             return revPtr

-- |@'addRevoked' crl revoked@ add the certificate to the revocation
-- list.
addRevoked :: CRL -> RevokedCertificate -> IO ()
addRevoked crl revoked
    = withCRLPtr crl $ \ crlPtr ->
      do revPtr <- newRevoked revoked
         ret    <- _add0_revoked crlPtr revPtr
         case ret of
           1 -> return ()
           _ -> freeRevoked revPtr >> raiseOpenSSLError

-- |@'getRevoked' crl serial@ looks up the corresponding revocation.
getRevoked :: CRL -> Integer -> IO (Maybe RevokedCertificate)
#if OPENSSL_VERSION_NUMBER >= 0x10000000
getRevoked crl serial =
  withCRLPtr crl  $ \crlPtr ->
  alloca          $ \revPtr ->
  withASN1Integer serial $ \serialPtr -> do
    r <- _get0_by_serial crlPtr revPtr serialPtr
    if r == 1
      then fmap Just $ peek revPtr >>= peekRevoked
      else return Nothing
#else
getRevoked crl serial = find p `fmap` getRevokedList crl
    where
      p :: RevokedCertificate -> Bool
      p = ((==) serial) . revSerialNumber
#endif

-- |@'sortCRL' crl@ sorts the certificates in the revocation list.
sortCRL :: CRL -> IO ()
sortCRL crl
    = withCRLPtr crl $ \ crlPtr ->
        _sort crlPtr >>= failIf_ (/= 1)
