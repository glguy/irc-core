{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK prune             #-}
-- |An interface to PKCS#7 structure and S\/MIME message.
module OpenSSL.PKCS7
    ( -- * Types
      Pkcs7
    , PKCS7 -- private
    , Pkcs7Flag(..)
    , Pkcs7VerifyStatus(..)
    , wrapPkcs7Ptr -- private
    , withPkcs7Ptr -- private

      -- * Encryption and Signing
    , pkcs7Sign
    , pkcs7Verify
    , pkcs7Encrypt
    , pkcs7Decrypt

      -- * S\/MIME
    , writeSmime
    , readSmime
    )
    where
#include "HsOpenSSL.h"
import           Data.List
import           Data.Traversable
import           Data.Typeable
import           Foreign
import           Foreign.C
import           OpenSSL.BIO
import           OpenSSL.EVP.Cipher hiding (cipher)
import           OpenSSL.EVP.PKey
import           OpenSSL.EVP.Internal
import           OpenSSL.Stack
import           OpenSSL.Utils
import           OpenSSL.X509
import           OpenSSL.X509.Store


{- PKCS#7 -------------------------------------------------------------------- -}

-- |@'Pkcs7'@ represents an abstract PKCS#7 structure. The concrete
-- type of structure is hidden in the object: such polymorphism isn't
-- very haskellish but please get it out of your mind since OpenSSL is
-- written in C.
newtype Pkcs7 = Pkcs7 (ForeignPtr PKCS7)
data    PKCS7

-- |@'Pkcs7Flag'@ is a set of flags that are used in many operations
-- related to PKCS#7.
data Pkcs7Flag = Pkcs7Text
               | Pkcs7NoCerts
               | Pkcs7NoSigs
               | Pkcs7NoChain
               | Pkcs7NoIntern
               | Pkcs7NoVerify
               | Pkcs7Detached
               | Pkcs7Binary
               | Pkcs7NoAttr
               | Pkcs7NoSmimeCap
               | Pkcs7NoOldMimeType
               | Pkcs7CRLFEOL
                 deriving (Show, Eq, Typeable)

flagToInt :: Pkcs7Flag -> CInt
flagToInt Pkcs7Text          = #const PKCS7_TEXT
flagToInt Pkcs7NoCerts       = #const PKCS7_NOCERTS
flagToInt Pkcs7NoSigs        = #const PKCS7_NOSIGS
flagToInt Pkcs7NoChain       = #const PKCS7_NOCHAIN
flagToInt Pkcs7NoIntern      = #const PKCS7_NOINTERN
flagToInt Pkcs7NoVerify      = #const PKCS7_NOVERIFY
flagToInt Pkcs7Detached      = #const PKCS7_DETACHED
flagToInt Pkcs7Binary        = #const PKCS7_BINARY
flagToInt Pkcs7NoAttr        = #const PKCS7_NOATTR
flagToInt Pkcs7NoSmimeCap    = #const PKCS7_NOSMIMECAP
flagToInt Pkcs7NoOldMimeType = #const PKCS7_NOOLDMIMETYPE
flagToInt Pkcs7CRLFEOL       = #const PKCS7_CRLFEOL

-- |@'Pkcs7VerifyStatus'@ represents a result of PKCS#7
-- verification. See 'pkcs7Verify'.
data Pkcs7VerifyStatus
    = Pkcs7VerifySuccess (Maybe String) -- ^ Nothing if the PKCS#7
                                        --   signature was a detached
                                        --   signature, and @Just content@
                                        --   if it wasn't.
    | Pkcs7VerifyFailure
      deriving (Show, Eq, Typeable)


flagListToInt :: [Pkcs7Flag] -> CInt
flagListToInt = foldl' (.|.) 0 . map flagToInt


foreign import ccall "&PKCS7_free"
        _free :: FunPtr (Ptr PKCS7 -> IO ())

foreign import ccall "HsOpenSSL_PKCS7_is_detached"
        _is_detached :: Ptr PKCS7 -> IO CLong

foreign import ccall "PKCS7_sign"
        _sign :: Ptr X509_ -> Ptr EVP_PKEY -> Ptr STACK -> Ptr BIO_ -> CInt -> IO (Ptr PKCS7)

foreign import ccall "PKCS7_verify"
        _verify :: Ptr PKCS7 -> Ptr STACK -> Ptr X509_STORE -> Ptr BIO_ -> Ptr BIO_ -> CInt -> IO CInt

foreign import ccall "PKCS7_encrypt"
        _encrypt :: Ptr STACK -> Ptr BIO_ -> Ptr EVP_CIPHER -> CInt -> IO (Ptr PKCS7)

foreign import ccall "PKCS7_decrypt"
        _decrypt :: Ptr PKCS7 -> Ptr EVP_PKEY -> Ptr X509_ -> Ptr BIO_ -> CInt -> IO CInt


wrapPkcs7Ptr :: Ptr PKCS7 -> IO Pkcs7
wrapPkcs7Ptr = fmap Pkcs7 . newForeignPtr _free


withPkcs7Ptr :: Pkcs7 -> (Ptr PKCS7 -> IO a) -> IO a
withPkcs7Ptr (Pkcs7 pkcs7) = withForeignPtr pkcs7


isDetachedSignature :: Pkcs7 -> IO Bool
isDetachedSignature pkcs7
    = withPkcs7Ptr pkcs7 $ \ pkcs7Ptr ->
      fmap (== 1) (_is_detached pkcs7Ptr)


pkcs7Sign' :: KeyPair key => X509 -> key -> [X509] -> BIO -> [Pkcs7Flag] -> IO Pkcs7
pkcs7Sign' signCert pkey certs input flagList
    = withX509Ptr signCert $ \ signCertPtr ->
      withPKeyPtr' pkey    $ \ pkeyPtr     ->
      withX509Stack certs  $ \ certStack   ->
      withBioPtr input     $ \ inputPtr    ->
      _sign signCertPtr pkeyPtr certStack inputPtr (flagListToInt flagList)
           >>= failIfNull
           >>= wrapPkcs7Ptr

-- |@'pkcs7Sign'@ creates a PKCS#7 signedData structure.
pkcs7Sign :: KeyPair key =>
             X509        -- ^ certificate to sign with
          -> key         -- ^ corresponding private key
          -> [X509]      -- ^ optional additional set of certificates
                         --   to include in the PKCS#7 structure (for
                         --   example any intermediate CAs in the
                         --   chain)
          -> String      -- ^ data to be signed
          -> [Pkcs7Flag] -- ^ An optional set of flags:
                         -- 
                         --   ['Pkcs7Text'] Many S\/MIME clients
                         --   expect the signed content to include
                         --   valid MIME headers. If the 'Pkcs7Text'
                         --   flag is set MIME headers for type
                         --   \"text\/plain\" are prepended to the
                         --   data.
                         --
                         --   ['Pkcs7NoCerts'] If 'Pkcs7NoCerts' is
                         --   set the signer's certificate will not be
                         --   included in the PKCS#7 structure, the
                         --   signer's certificate must still be
                         --   supplied in the parameter though. This
                         --   can reduce the size of the signature if
                         --   the signer's certificate can be obtained
                         --   by other means: for example a previously
                         --   signed message.
                         --
                         --   ['Pkcs7Detached'] The data being signed
                         --   is included in the PKCS#7 structure,
                         --   unless 'Pkcs7Detached' is set in which
                         --   case it is ommited. This is used for
                         --   PKCS#7 detached signatures which are
                         --   used in S\/MIME plaintext signed message
                         --   for example.
                         --
                         --   ['Pkcs7Binary'] Normally the supplied
                         --   content is translated into MIME
                         --   canonical format (as required by the
                         --   S\/MIME specifications) but if
                         --   'Pkcs7Binary' is set no translation
                         --   occurs. This option should be uesd if
                         --   the supplied data is in binary format
                         --   otherwise the translation will corrupt
                         --   it.
                         --
                         --   ['Pkcs7NoAttr']
                         --
                         --   ['Pkcs7NoSmimeCap'] The signedData
                         --   structure includes several PKCS#7
                         --   authenticatedAttributes including the
                         --   signing time, the PKCS#7 content type
                         --   and the supported list of ciphers in an
                         --   SMIMECapabilities attribute. If
                         --   'Pkcs7NoAttr' is set then no
                         --   authenticatedAttributes will be used. If
                         --   Pkcs7NoSmimeCap is set then just the
                         --   SMIMECapabilities are omitted.
          -> IO Pkcs7
pkcs7Sign signCert pkey certs input flagList
    = do mem <- newConstMem input
         pkcs7Sign' signCert pkey certs mem flagList


pkcs7Verify' :: Pkcs7 -> [X509] -> X509Store -> Maybe BIO -> [Pkcs7Flag] -> IO (Maybe BIO, Bool)
pkcs7Verify' pkcs7 certs store inData flagList
    = withPkcs7Ptr pkcs7     $ \ pkcs7Ptr  ->
      withX509Stack certs    $ \ certStack ->
      withX509StorePtr store $ \ storePtr  ->
      withBioPtr' inData     $ \ inDataPtr ->
      do isDetached <- isDetachedSignature pkcs7
         outData    <- if isDetached then
                           return Nothing
                       else
                           fmap Just newMem
         withBioPtr' outData $ \ outDataPtr ->
             _verify pkcs7Ptr certStack storePtr inDataPtr outDataPtr (flagListToInt flagList)
                  >>= interpret outData
    where
      interpret :: Maybe BIO -> CInt -> IO (Maybe BIO, Bool)
      interpret bio 1 = return (bio    , True )
      interpret _   _ = return (Nothing, False)

-- |@'pkcs7Verify'@ verifies a PKCS#7 signedData structure.
pkcs7Verify :: Pkcs7           -- ^ A PKCS#7 structure to verify.
            -> [X509]          -- ^ Set of certificates in which to
                               --   search for the signer's
                               --   certificate.
            -> X509Store       -- ^ Trusted certificate store (used
                               --   for chain verification).
            -> Maybe String    -- ^ Signed data if the content is not
                               --   present in the PKCS#7 structure
                               --   (that is it is detached).
            -> [Pkcs7Flag]     -- ^ An optional set of flags:
                               -- 
                               --   ['Pkcs7NoIntern'] If
                               --   'Pkcs7NoIntern' is set the
                               --   certificates in the message itself
                               --   are not searched when locating the
                               --   signer's certificate. This means
                               --   that all the signers certificates
                               --   must be in the second argument
                               --   (['X509']).
                               --
                               --   ['Pkcs7Text'] If the 'Pkcs7Text'
                               --   flag is set MIME headers for type
                               --   \"text\/plain\" are deleted from
                               --   the content. If the content is not
                               --   of type \"text\/plain\" then an
                               --   error is returned.
                               --
                               --   ['Pkcs7NoVerify'] If
                               --   'Pkcs7NoVerify' is set the
                               --   signer's certificates are not
                               --   chain verified.
                               --
                               --   ['Pkcs7NoChain'] If 'Pkcs7NoChain'
                               --   is set then the certificates
                               --   contained in the message are not
                               --   used as untrusted CAs. This means
                               --   that the whole verify chain (apart
                               --   from the signer's certificate)
                               --   must be contained in the trusted
                               --   store.
                               --
                               --   ['Pkcs7NoSigs'] If 'Pkcs7NoSigs'
                               --   is set then the signatures on the
                               --   data are not checked.
            -> IO Pkcs7VerifyStatus
pkcs7Verify pkcs7 certs store inData flagList
    = do inDataBio               <- forM inData newConstMem
         (outDataBio, isSuccess) <- pkcs7Verify' pkcs7 certs store inDataBio flagList
         if isSuccess then
             do outData <- forM outDataBio bioRead
                return $ Pkcs7VerifySuccess outData
           else
             return Pkcs7VerifyFailure


pkcs7Encrypt' :: [X509] -> BIO -> Cipher -> [Pkcs7Flag] -> IO Pkcs7
pkcs7Encrypt' certs input cipher flagList
    = withX509Stack certs  $ \ certsPtr  ->
      withBioPtr    input  $ \ inputPtr  ->
      withCipherPtr cipher $ \ cipherPtr ->
      _encrypt certsPtr inputPtr cipherPtr (flagListToInt flagList)
           >>= failIfNull
           >>= wrapPkcs7Ptr

-- |@'pkcs7Encrypt'@ creates a PKCS#7 envelopedData structure.
pkcs7Encrypt :: [X509]      -- ^ A list of recipient certificates.
             -> String      -- ^ The content to be encrypted.
             -> Cipher      -- ^ The symmetric cipher to use.
             -> [Pkcs7Flag] -- ^ An optional set of flags:
                            --
                            --   ['Pkcs7Text'] If the 'Pkcs7Text' flag
                            --   is set MIME headers for type
                            --   \"text\/plain\" are prepended to the
                            --   data.
                            --
                            --   ['Pkcs7Binary'] Normally the supplied
                            --   content is translated into MIME
                            --   canonical format (as required by the
                            --   S\/MIME specifications) if
                            --   'Pkcs7Binary' is set no translation
                            --   occurs. This option should be used if
                            --   the supplied data is in binary format
                            --   otherwise the translation will
                            --   corrupt it. If 'Pkcs7Binary' is set
                            --   then 'Pkcs7Text' is ignored.
             -> IO Pkcs7
pkcs7Encrypt certs input cipher flagList
    = do mem <- newConstMem input
         pkcs7Encrypt' certs mem cipher flagList


pkcs7Decrypt' :: KeyPair key => Pkcs7 -> key -> X509 -> BIO -> [Pkcs7Flag] -> IO ()
pkcs7Decrypt' pkcs7 pkey cert output flagList
    = withPkcs7Ptr pkcs7  $ \ pkcs7Ptr  ->
      withPKeyPtr' pkey   $ \ pkeyPtr   ->
      withX509Ptr  cert   $ \ certPtr   ->
      withBioPtr   output $ \ outputPtr ->
      _decrypt pkcs7Ptr pkeyPtr certPtr outputPtr (flagListToInt flagList)
           >>= failIf (/= 1)
           >>  return ()

-- |@'pkcs7Decrypt'@ decrypts content from PKCS#7 envelopedData
-- structure.
pkcs7Decrypt :: KeyPair key =>
                Pkcs7       -- ^ The PKCS#7 structure to decrypt.
             -> key         -- ^ The private key of the recipient.
             -> X509        -- ^ The recipient's certificate.
             -> [Pkcs7Flag] -- ^ An optional set of flags:
                            --
                            --   ['Pkcs7Text'] If the 'Pkcs7Text' flag
                            --   is set MIME headers for type
                            --   \"text\/plain\" are deleted from the
                            --   content. If the content is not of
                            --   type \"text\/plain\" then an error is
                            --   thrown.
             -> IO String   -- ^ The decrypted content.
pkcs7Decrypt pkcs7 pkey cert flagList
    = do mem <- newMem
         pkcs7Decrypt' pkcs7 pkey cert mem flagList
         bioRead mem


{- S/MIME -------------------------------------------------------------------- -}

foreign import ccall unsafe "SMIME_write_PKCS7"
        _SMIME_write_PKCS7 :: Ptr BIO_ -> Ptr PKCS7 -> Ptr BIO_ -> CInt -> IO CInt

foreign import ccall unsafe "SMIME_read_PKCS7"
        _SMIME_read_PKCS7 :: Ptr BIO_ -> Ptr (Ptr BIO_) -> IO (Ptr PKCS7)

-- |@'writeSmime'@ writes PKCS#7 structure to S\/MIME message.
writeSmime :: Pkcs7        -- ^ A PKCS#7 structure to be written.
           -> Maybe String -- ^ If cleartext signing
                           --   (multipart\/signed) is being used then
                           --   the signed data must be supplied here.
           -> [Pkcs7Flag]  -- ^ An optional set of flags:
                           --
                           --   ['Pkcs7Detached'] If 'Pkcs7Detached'
                           --   is set then cleartext signing will be
                           --   used, this option only makes sense for
                           --   signedData where 'Pkcs7Detached' is
                           --   also set when 'pkcs7Sign' is also
                           --   called.
                           --
                           --   ['Pkcs7Text'] If the 'Pkcs7Text' flag
                           --   is set MIME headers for type
                           --   \"text\/plain\" are added to the
                           --   content, this only makes sense if
                           --   'Pkcs7Detached' is also set.
           -> IO String    -- ^ The result S\/MIME message.
writeSmime pkcs7 dataStr flagList
    = do outBio  <- newMem
         dataBio <- forM dataStr newConstMem
         writeSmime' outBio pkcs7 dataBio flagList
         bioRead outBio


writeSmime' :: BIO -> Pkcs7 -> Maybe BIO -> [Pkcs7Flag] -> IO ()
writeSmime' outBio pkcs7 dataBio flagList
    = withBioPtr   outBio  $ \ outBioPtr  ->
      withPkcs7Ptr pkcs7   $ \ pkcs7Ptr   ->
      withBioPtr'  dataBio $ \ dataBioPtr ->
      _SMIME_write_PKCS7 outBioPtr pkcs7Ptr dataBioPtr (flagListToInt flagList)
           >>= failIf (/= 1)
           >>  return ()

-- |@'readSmime'@ parses S\/MIME message.
readSmime :: String -- ^ The message to be read.
          -> IO (Pkcs7, Maybe String) -- ^ (The result PKCS#7
                                      --   structure, @Just content@
                                      --   if the PKCS#7 structure was
                                      --   a cleartext signature and
                                      --   @Nothing@ if it wasn't.)
readSmime input
    = do inBio           <- newConstMem input
         (pkcs7, outBio) <- readSmime' inBio
         output          <- forM outBio bioRead
         return (pkcs7, output)


readSmime' :: BIO -> IO (Pkcs7, Maybe BIO)
readSmime' inBio
    = withBioPtr inBio $ \ inBioPtr     ->
      alloca           $ \ outBioPtrPtr ->
      do poke outBioPtrPtr nullPtr

         pkcs7     <- _SMIME_read_PKCS7 inBioPtr outBioPtrPtr
                      >>= failIfNull
                      >>= wrapPkcs7Ptr
         outBioPtr <- peek outBioPtrPtr
         outBio    <- if outBioPtr == nullPtr then
                          return Nothing
                      else
                          fmap Just (wrapBioPtr outBioPtr)

         return (pkcs7, outBio)
