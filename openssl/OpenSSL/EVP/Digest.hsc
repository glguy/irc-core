{-# LANGUAGE ForeignFunctionInterface #-}
-- |An interface to message digest algorithms.
module OpenSSL.EVP.Digest
    ( Digest
    , getDigestByName
    , getDigestNames

    , digest
    , digestBS
    , digestLBS

    , hmacBS
    , pkcs5_pbkdf2_hmac_sha1
    )
    where
#include "HsOpenSSL.h"
import Data.ByteString.Internal (create)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Applicative ((<$>))
import Foreign.C.String (CString, withCString)
#if MIN_VERSION_base(4,5,0)
import Foreign.C.Types (CChar(..), CInt(..), CSize(..), CUInt(..))
#else
import Foreign.C.Types (CChar, CInt, CSize, CUInt)
#endif
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import OpenSSL.EVP.Internal
import OpenSSL.Objects
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "EVP_get_digestbyname"
        _get_digestbyname :: CString -> IO (Ptr EVP_MD)

-- |@'getDigestByName' name@ returns a message digest algorithm whose
-- name is @name@. If no algorithms are found, the result is
-- @Nothing@.
getDigestByName :: String -> IO (Maybe Digest)
getDigestByName name
    = withCString name $ \ namePtr ->
      do ptr <- _get_digestbyname namePtr
         if ptr == nullPtr then
             return Nothing
           else
             return $ Just $ Digest ptr

-- |@'getDigestNames'@ returns a list of name of message digest
-- algorithms.
getDigestNames :: IO [String]
getDigestNames = getObjNames MDMethodType True

{- digest -------------------------------------------------------------------- -}

-- |@'digest'@ digests a stream of data. The string must
-- not contain any letters which aren't in the range of U+0000 -
-- U+00FF.
digest :: Digest -> String -> String
{-# DEPRECATED digest "Use digestBS or digestLBS instead." #-}
digest md input
    = B8.unpack $ digestLBS md $ L8.pack input

-- |@'digestBS'@ digests a chunk of data.
digestBS :: Digest -> B8.ByteString -> B8.ByteString
digestBS md input
    = unsafePerformIO $ digestStrictly md input >>= digestFinalBS

-- |@'digestLBS'@ digests a stream of data.
digestLBS :: Digest -> L8.ByteString -> B8.ByteString
digestLBS md input
    = unsafePerformIO $ digestLazily md input >>= digestFinalBS

{- HMAC ---------------------------------------------------------------------- -}

foreign import ccall unsafe "HMAC"
        _HMAC :: Ptr EVP_MD -> Ptr CChar -> CInt -> Ptr CChar -> CSize
              -> Ptr CChar -> Ptr CUInt -> IO ()

-- | Perform a private key signing using the HMAC template with a given hash
hmacBS :: Digest  -- ^ the hash function to use in the HMAC calculation
       -> B8.ByteString  -- ^ the HMAC key
       -> B8.ByteString  -- ^ the data to be signed
       -> B8.ByteString  -- ^ resulting HMAC
hmacBS (Digest md) key input =
  unsafePerformIO $
  allocaArray (#const EVP_MAX_MD_SIZE) $ \bufPtr ->
  alloca $ \bufLenPtr ->
  unsafeUseAsCStringLen key $ \(keydata, keylen) ->
  unsafeUseAsCStringLen input $ \(inputdata, inputlen) -> do
     _HMAC md
       keydata (fromIntegral keylen) inputdata (fromIntegral inputlen)
       bufPtr bufLenPtr
     bufLen <- fromIntegral <$> peek bufLenPtr
     B8.packCStringLen (bufPtr, bufLen)

-- | Calculate a PKCS5-PBKDF2 SHA1-HMAC suitable for password hashing.
pkcs5_pbkdf2_hmac_sha1 :: B8.ByteString -- ^ password
                       -> B8.ByteString -- ^ salt
                       -> Int           -- ^ iterations
                       -> Int           -- ^ destination key length
                       -> B8.ByteString -- ^ destination key
pkcs5_pbkdf2_hmac_sha1 pass salt iter dkeylen =
  unsafePerformIO $
  unsafeUseAsCStringLen pass $ \(passdata, passlen) ->
  unsafeUseAsCStringLen salt $ \(saltdata, saltlen) ->
  create dkeylen $ \dkeydata ->
      _PKCS5_PBKDF2_HMAC_SHA1
           passdata (fromIntegral passlen)
           saltdata (fromIntegral saltlen)
           (fromIntegral iter) (fromIntegral dkeylen) (castPtr dkeydata)
      >> return ()

foreign import ccall unsafe "PKCS5_PBKDF2_HMAC_SHA1"
  _PKCS5_PBKDF2_HMAC_SHA1 :: Ptr CChar -> CInt
                          -> Ptr CChar -> CInt
                          -> CInt -> CInt -> Ptr CChar
                          -> IO CInt

