{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | This module interfaces to some of the OpenSSL ciphers without using
--   EVP (see OpenSSL.EVP.Cipher). The EVP ciphers are easier to use,
--   however, in some cases you cannot do without using the OpenSSL
--   fuctions directly.
--
--   One of these cases (and the motivating example
--   for this module) is that the EVP CBC functions try to encode the
--   length of the input string in the output (thus hiding the fact that the
--   cipher is, in fact, block based and needs padding). This means that the
--   EVP CBC functions cannot, in some cases, interface with other users
--   which don't use that system (like SSH).
module OpenSSL.Cipher
    ( Mode(..)
    , AESCtx
    , newAESCtx
    , aesCBC
    , aesCTR)
    where
#include "HsOpenSSL.h"
#include "openssl/aes.h"
import           Control.Monad (when, unless)
import           Data.IORef
import           Foreign
import           Foreign.C.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import           OpenSSL.Utils

data Mode = Encrypt | Decrypt deriving (Eq, Show)

modeToInt :: Num a => Mode -> a
modeToInt Encrypt = 1
modeToInt Decrypt = 0

data AES_KEY
data AESCtx = AESCtx
                (ForeignPtr AES_KEY)  -- the key schedule
                (ForeignPtr CUChar)   -- the IV / counter
                (ForeignPtr CUChar)   -- the encrypted counter (CTR mode)
                (IORef CUInt)         -- the number of bytes of the encrypted counter used
                Mode

foreign import ccall unsafe "memcpy"
        _memcpy :: Ptr CUChar -> Ptr CChar -> CSize -> IO (Ptr ())

foreign import ccall unsafe "memset"
        _memset :: Ptr CUChar -> CChar -> CSize -> IO ()

foreign import ccall unsafe "AES_set_encrypt_key"
        _AES_set_encrypt_key :: Ptr CChar -> CInt -> Ptr AES_KEY -> IO CInt
foreign import ccall unsafe "AES_set_decrypt_key"
        _AES_set_decrypt_key :: Ptr CChar -> CInt -> Ptr AES_KEY -> IO CInt

foreign import ccall unsafe "AES_cbc_encrypt"
        _AES_cbc_encrypt :: Ptr CChar -> Ptr Word8 -> CULong -> Ptr AES_KEY -> Ptr CUChar -> CInt -> IO ()

foreign import ccall unsafe "AES_ctr128_encrypt"
        _AES_ctr_encrypt :: Ptr CChar -> Ptr Word8 -> CULong -> Ptr AES_KEY -> Ptr CUChar -> Ptr CUChar -> Ptr CUInt -> IO ()

foreign import ccall unsafe "&free"
        _free :: FunPtr (Ptr a -> IO ())

-- | Construct a new context which holds the key schedule and IV.
newAESCtx :: Mode  -- ^ For CTR mode, this must always be Encrypt
          -> BS.ByteString  -- ^ Key: 128, 192 or 256 bits long
          -> BS.ByteString  -- ^ IV: 16 bytes long
          -> IO AESCtx
newAESCtx mode key iv = do
  let keyLen = BS.length key * 8
  unless (any (keyLen ==) [128, 192, 256]) $ fail "Bad AES key length"
  when (BS.length iv /= 16) $ fail "Bad AES128 iv length"
  ctx <- mallocForeignPtrBytes (#size AES_KEY)
  withForeignPtr ctx $ \ctxPtr ->
    BS.useAsCStringLen key (\(ptr, _) ->
      case mode of
           Encrypt -> _AES_set_encrypt_key ptr (fromIntegral keyLen) ctxPtr >>= failIf_ (/= 0)
           Decrypt -> _AES_set_decrypt_key ptr (fromIntegral keyLen) ctxPtr >>= failIf_ (/= 0))
  ivbytes <- mallocForeignPtrBytes 16
  ecounter <- mallocForeignPtrBytes 16
  nref <- newIORef 0
  withForeignPtr ecounter (\ecptr -> _memset ecptr 0 16)
  withForeignPtr ivbytes $ \ivPtr ->
    BS.useAsCStringLen iv $ \(ptr, _) ->
    do _ <- _memcpy ivPtr ptr 16
       return $ AESCtx ctx ivbytes ecounter nref mode

-- | Encrypt some number of blocks using CBC. This is an IO function because
--   the context is destructivly updated.
aesCBC :: AESCtx  -- ^ context
       -> BS.ByteString  -- ^ input, must be multiple of block size (16 bytes)
       -> IO BS.ByteString
aesCBC (AESCtx ctx iv _ _ mode) input = do
  when (BS.length input `mod` 16 /= 0) $ fail "Bad input length to aesCBC"
  withForeignPtr ctx $ \ctxPtr ->
    withForeignPtr iv $ \ivPtr ->
    BS.useAsCStringLen input $ \(ptr, len) ->
    BSI.create (BS.length input) $ \out ->
    _AES_cbc_encrypt ptr out (fromIntegral len) ctxPtr ivPtr $ modeToInt mode

-- | Encrypt some number of bytes using CTR mode. This is an IO function
--   because the context is destructivly updated.
aesCTR :: AESCtx  -- ^ context
       -> BS.ByteString  -- ^ input, any number of bytes
       -> IO BS.ByteString
aesCTR (AESCtx _   _  _        _    Decrypt) _     = fail "the context mode must be Encrypt"
aesCTR (AESCtx ctx iv ecounter nref Encrypt) input =
  withForeignPtr ctx $ \ctxPtr ->
    withForeignPtr iv $ \ivPtr ->
    withForeignPtr ecounter $ \ecptr ->
    BS.useAsCStringLen input $ \(ptr, len) ->
    BSI.create (BS.length input) $ \out ->
    alloca $ \nptr -> do
      n <- readIORef nref
      poke nptr n
      _AES_ctr_encrypt ptr out (fromIntegral len) ctxPtr ivPtr ecptr nptr
      n' <- peek nptr
      writeIORef nref n'
