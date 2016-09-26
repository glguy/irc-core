{-# LANGUAGE ForeignFunctionInterface #-}
-- | PRNG services
--   See <http://www.openssl.org/docs/crypto/rand.html>
--   For random Integer generation, see "OpenSSL.BN"
module OpenSSL.Random
    ( -- * Random byte generation
      randBytes
    , prandBytes
    , add
    ) where
import           Foreign
import           Foreign.C.Types
import qualified Data.ByteString as BS
import           OpenSSL.Utils

foreign import ccall unsafe "RAND_bytes"
        _RAND_bytes :: Ptr CChar -> CInt -> IO CInt

foreign import ccall unsafe "RAND_pseudo_bytes"
        _RAND_pseudo_bytes :: Ptr CChar -> CInt -> IO ()

foreign import ccall unsafe "RAND_add"
        _RAND_add :: Ptr CChar -> CInt -> CInt -> IO ()

-- | Return a bytestring consisting of the given number of strongly random
--   bytes
randBytes :: Int  -- ^ the number of bytes requested
          -> IO BS.ByteString
randBytes n =
  allocaArray n $ \bufPtr ->
  do _RAND_bytes bufPtr (fromIntegral n) >>= failIf_ (/= 1)
     BS.packCStringLen (bufPtr, n)

-- | Return a bytestring consisting of the given number of pseudo random
--   bytes
prandBytes :: Int  -- ^ the number of bytes requested
           -> IO BS.ByteString
prandBytes n =
  allocaArray n $ \bufPtr ->
  do _RAND_pseudo_bytes bufPtr (fromIntegral n)
     BS.packCStringLen (bufPtr, n)

-- | Add data to the entropy pool. It's safe to add sensitive information
--   (e.g. user passwords etc) to the pool. Also, adding data with an entropy
--   of 0 can never hurt.
add :: BS.ByteString  -- ^ random data to be added to the pool
    -> Int  -- ^ the number of bits of entropy in the first argument
    -> IO ()
add bs entropy =
  BS.useAsCStringLen bs $ \(ptr, len) ->
  _RAND_add ptr (fromIntegral len) (fromIntegral entropy)
