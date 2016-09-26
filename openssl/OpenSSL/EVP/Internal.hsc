{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module OpenSSL.EVP.Internal (
    Cipher(..),
    EVP_CIPHER,
    withCipherPtr,

    cipherIvLength,

    CipherCtx(..),
    EVP_CIPHER_CTX,
    newCipherCtx,
    withCipherCtxPtr,
    withNewCipherCtxPtr,

    CryptoMode(..),
    cipherInitBS,
    cipherUpdateBS,
    cipherFinalBS,
    cipherStrictly,
    cipherLazily,

    Digest(..),
    EVP_MD,
    withMDPtr,

    DigestCtx(..),
    EVP_MD_CTX,
    withDigestCtxPtr,

    digestUpdateBS,
    digestFinalBS,
    digestFinal,
    digestStrictly,
    digestLazily,

    VaguePKey(..),
    EVP_PKEY,
    PKey(..),
    createPKey,
    wrapPKeyPtr,
    withPKeyPtr,
    withPKeyPtr',
    unsafePKeyToPtr,
    touchPKey
  ) where

#include "HsOpenSSL.h"

import qualified Data.ByteString.Internal as B8
import qualified Data.ByteString.Unsafe as B8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.Internal as L8
import Control.Applicative ((<$>))
import Control.Exception (mask, mask_, bracket_, onException)
import Foreign.C.Types (CChar)
#if MIN_VERSION_base(4,5,0)
import Foreign.C.Types (CInt(..), CUInt(..), CSize(..))
#else
import Foreign.C.Types (CInt, CUInt, CSize)
#endif
import Foreign.Ptr (Ptr, castPtr, FunPtr)
import Foreign.C.String (CString, peekCStringLen)
import Foreign.ForeignPtr
#if MIN_VERSION_base(4,4,0)
import Foreign.ForeignPtr.Unsafe as Unsafe
#else
import Foreign.ForeignPtr as Unsafe
#endif
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray)
import System.IO.Unsafe (unsafeInterleaveIO)
import OpenSSL.Utils

{- EVP_CIPHER ---------------------------------------------------------------- -}

-- |@Cipher@ is an opaque object that represents an algorithm of
-- symmetric cipher.
newtype Cipher     = Cipher (Ptr EVP_CIPHER)
data    EVP_CIPHER

withCipherPtr :: Cipher -> (Ptr EVP_CIPHER -> IO a) -> IO a
withCipherPtr (Cipher cipherPtr) f = f cipherPtr

foreign import ccall unsafe "HsOpenSSL_EVP_CIPHER_iv_length"
        _iv_length :: Ptr EVP_CIPHER -> CInt

cipherIvLength :: Cipher -> Int
cipherIvLength (Cipher cipherPtr) = fromIntegral $ _iv_length cipherPtr

{- EVP_CIPHER_CTX ------------------------------------------------------------ -}

newtype CipherCtx      = CipherCtx (ForeignPtr EVP_CIPHER_CTX)
data    EVP_CIPHER_CTX

foreign import ccall unsafe "EVP_CIPHER_CTX_init"
  _cipher_ctx_init :: Ptr EVP_CIPHER_CTX -> IO ()

foreign import ccall unsafe "&EVP_CIPHER_CTX_cleanup"
  _cipher_ctx_cleanup :: FunPtr (Ptr EVP_CIPHER_CTX -> IO ())

foreign import ccall unsafe "EVP_CIPHER_CTX_cleanup"
  _cipher_ctx_cleanup' :: Ptr EVP_CIPHER_CTX -> IO ()

foreign import ccall unsafe "HsOpenSSL_EVP_CIPHER_CTX_block_size"
  _cipher_ctx_block_size :: Ptr EVP_CIPHER_CTX -> CInt

newCipherCtx :: IO CipherCtx
newCipherCtx = do
  ctx <- mallocForeignPtrBytes (#size EVP_CIPHER_CTX)
  mask_ $ do
    withForeignPtr ctx _cipher_ctx_init
    addForeignPtrFinalizer _cipher_ctx_cleanup ctx
  return $ CipherCtx ctx

withCipherCtxPtr :: CipherCtx -> (Ptr EVP_CIPHER_CTX -> IO a) -> IO a
withCipherCtxPtr (CipherCtx ctx) = withForeignPtr ctx

withNewCipherCtxPtr :: (Ptr EVP_CIPHER_CTX -> IO a) -> IO a
withNewCipherCtxPtr f =
  allocaBytes (#size EVP_CIPHER_CTX) $ \ptr ->
    bracket_ (_cipher_ctx_init ptr) (_cipher_ctx_cleanup' ptr) (f ptr)

{- encrypt/decrypt ----------------------------------------------------------- -}

-- |@CryptoMode@ represents instruction to 'cipher' and such like.
data CryptoMode = Encrypt | Decrypt

fromCryptoMode :: Num a => CryptoMode -> a
fromCryptoMode Encrypt = 1
fromCryptoMode Decrypt = 0

foreign import ccall unsafe "EVP_CipherInit"
        _CipherInit :: Ptr EVP_CIPHER_CTX
                    -> Ptr EVP_CIPHER
                    -> CString
                    -> CString
                    -> CInt
                    -> IO CInt

cipherInitBS :: Cipher
             -> B8.ByteString -- ^ key
             -> B8.ByteString -- ^ IV
             -> CryptoMode
             -> IO CipherCtx
cipherInitBS (Cipher c) key iv mode
    = do ctx <- newCipherCtx
         withCipherCtxPtr ctx $ \ ctxPtr ->
             B8.unsafeUseAsCString key $ \ keyPtr ->
                 B8.unsafeUseAsCString iv $ \ ivPtr ->
                     _CipherInit ctxPtr c keyPtr ivPtr (fromCryptoMode mode)
                          >>= failIf_ (/= 1)
         return ctx

foreign import ccall unsafe "EVP_CipherUpdate"
  _CipherUpdate :: Ptr EVP_CIPHER_CTX -> Ptr CChar -> Ptr CInt
                -> Ptr CChar -> CInt -> IO CInt

cipherUpdateBS :: CipherCtx -> B8.ByteString -> IO B8.ByteString
cipherUpdateBS ctx inBS =
  withCipherCtxPtr ctx $ \ctxPtr ->
    B8.unsafeUseAsCStringLen inBS $ \(inBuf, inLen) ->
      let len = inLen + fromIntegral (_cipher_ctx_block_size ctxPtr) - 1 in
        B8.createAndTrim len $ \outBuf ->
          alloca $ \outLenPtr ->
            _CipherUpdate ctxPtr (castPtr outBuf) outLenPtr inBuf
                          (fromIntegral inLen)
              >>= failIf (/= 1)
              >>  fromIntegral <$> peek outLenPtr

foreign import ccall unsafe "EVP_CipherFinal"
  _CipherFinal :: Ptr EVP_CIPHER_CTX -> Ptr CChar -> Ptr CInt -> IO CInt

cipherFinalBS :: CipherCtx -> IO B8.ByteString
cipherFinalBS ctx =
  withCipherCtxPtr ctx $ \ctxPtr ->
    let len = fromIntegral $ _cipher_ctx_block_size ctxPtr in
      B8.createAndTrim len $ \outBuf ->
        alloca $ \outLenPtr ->
          _CipherFinal ctxPtr (castPtr outBuf) outLenPtr
            >>= failIf (/= 1)
            >>  fromIntegral <$> peek outLenPtr

cipherStrictly :: CipherCtx -> B8.ByteString -> IO B8.ByteString
cipherStrictly ctx input = do
  output'  <- cipherUpdateBS ctx input
  output'' <- cipherFinalBS ctx
  return $ B8.append output' output''

cipherLazily :: CipherCtx -> L8.ByteString -> IO L8.ByteString
cipherLazily ctx (L8.Empty) =
  cipherFinalBS ctx >>= return . L8.fromChunks . return
cipherLazily ctx (L8.Chunk x xs) = do
  y  <- cipherUpdateBS ctx x
  ys <- unsafeInterleaveIO $ cipherLazily ctx xs
  return $ L8.Chunk y ys

{- EVP_MD -------------------------------------------------------------------- -}

-- |@Digest@ is an opaque object that represents an algorithm of
-- message digest.
newtype Digest = Digest (Ptr EVP_MD)
data    EVP_MD

withMDPtr :: Digest -> (Ptr EVP_MD -> IO a) -> IO a
withMDPtr (Digest mdPtr) f = f mdPtr

{- EVP_MD_CTX ---------------------------------------------------------------- -}

newtype DigestCtx  = DigestCtx (ForeignPtr EVP_MD_CTX)
data    EVP_MD_CTX

foreign import ccall unsafe "EVP_MD_CTX_init"
  _md_ctx_init :: Ptr EVP_MD_CTX -> IO ()

foreign import ccall unsafe "&EVP_MD_CTX_cleanup"
  _md_ctx_cleanup :: FunPtr (Ptr EVP_MD_CTX -> IO ())

newDigestCtx :: IO DigestCtx
newDigestCtx = do
  ctx <- mallocForeignPtrBytes (#size EVP_MD_CTX)
  mask_ $ do
    withForeignPtr ctx _md_ctx_init
    addForeignPtrFinalizer _md_ctx_cleanup ctx
  return $ DigestCtx ctx

withDigestCtxPtr :: DigestCtx -> (Ptr EVP_MD_CTX -> IO a) -> IO a
withDigestCtxPtr (DigestCtx ctx) = withForeignPtr ctx

{- digest -------------------------------------------------------------------- -}

foreign import ccall unsafe "EVP_DigestInit"
  _DigestInit :: Ptr EVP_MD_CTX -> Ptr EVP_MD -> IO CInt

digestInit :: Digest -> IO DigestCtx
digestInit (Digest md) = do
  ctx <- newDigestCtx
  withDigestCtxPtr ctx $ \ctxPtr ->
    _DigestInit ctxPtr md
      >>= failIf_ (/= 1)
      >>  return ctx

foreign import ccall unsafe "EVP_DigestUpdate"
  _DigestUpdate :: Ptr EVP_MD_CTX -> Ptr CChar -> CSize -> IO CInt

digestUpdateBS :: DigestCtx -> B8.ByteString -> IO ()
digestUpdateBS ctx bs =
  withDigestCtxPtr ctx $ \ctxPtr ->
    B8.unsafeUseAsCStringLen bs $ \(buf, len) ->
      _DigestUpdate ctxPtr buf (fromIntegral len)
        >>= failIf (/= 1)
        >>  return ()

foreign import ccall unsafe "EVP_DigestFinal"
  _DigestFinal :: Ptr EVP_MD_CTX -> Ptr CChar -> Ptr CUInt -> IO CInt

digestFinalBS :: DigestCtx -> IO B8.ByteString
digestFinalBS ctx =
  withDigestCtxPtr ctx $ \ctxPtr ->
    B8.createAndTrim (#const EVP_MAX_MD_SIZE) $ \bufPtr ->
      alloca $ \bufLenPtr -> do
        _DigestFinal ctxPtr (castPtr bufPtr) bufLenPtr >>= failIf_ (/= 1)
        fromIntegral <$> peek bufLenPtr

digestFinal :: DigestCtx -> IO String
digestFinal ctx =
  withDigestCtxPtr ctx $ \ctxPtr ->
    allocaArray (#const EVP_MAX_MD_SIZE) $ \bufPtr ->
      alloca $ \bufLenPtr -> do
        _DigestFinal ctxPtr bufPtr bufLenPtr >>= failIf_ (/= 1)
        bufLen <- fromIntegral <$> peek bufLenPtr
        peekCStringLen (bufPtr, bufLen)

digestStrictly :: Digest -> B8.ByteString -> IO DigestCtx
digestStrictly md input = do
  ctx <- digestInit md
  digestUpdateBS ctx input
  return ctx

digestLazily :: Digest -> L8.ByteString -> IO DigestCtx
digestLazily md lbs = do
  ctx <- digestInit md
  mapM_ (digestUpdateBS ctx) $ L8.toChunks lbs
  return ctx

{- EVP_PKEY ------------------------------------------------------------------ -}

-- | VaguePKey is a 'ForeignPtr' to 'EVP_PKEY', that is either public
-- key or a ker pair. We can't tell which at compile time.
newtype VaguePKey = VaguePKey (ForeignPtr EVP_PKEY)
data    EVP_PKEY

-- | Instances of class 'PKey' can be converted back and forth to
-- 'VaguePKey'.
class PKey k where
    -- | Wrap the key (i.g. RSA) into 'EVP_PKEY'.
    toPKey        :: k -> IO VaguePKey

    -- | Extract the concrete key from the 'EVP_PKEY'. Returns
    -- 'Nothing' if the type mismatches.
    fromPKey      :: VaguePKey -> IO (Maybe k)

    -- | Do the same as EVP_PKEY_size().
    pkeySize      :: k -> Int

    -- | Return the default digesting algorithm for the key.
    pkeyDefaultMD :: k -> IO Digest

foreign import ccall unsafe "EVP_PKEY_new"
  _pkey_new :: IO (Ptr EVP_PKEY)

foreign import ccall unsafe "&EVP_PKEY_free"
  _pkey_free :: FunPtr (Ptr EVP_PKEY -> IO ())

foreign import ccall unsafe "EVP_PKEY_free"
  _pkey_free' :: Ptr EVP_PKEY -> IO ()

wrapPKeyPtr :: Ptr EVP_PKEY -> IO VaguePKey
wrapPKeyPtr = fmap VaguePKey . newForeignPtr _pkey_free

createPKey :: (Ptr EVP_PKEY -> IO a) -> IO VaguePKey
createPKey f = mask $ \restore -> do
  ptr <- _pkey_new >>= failIfNull
  (restore $ f ptr >> return ()) `onException` _pkey_free' ptr
  wrapPKeyPtr ptr

withPKeyPtr :: VaguePKey -> (Ptr EVP_PKEY -> IO a) -> IO a
withPKeyPtr (VaguePKey pkey) = withForeignPtr pkey

withPKeyPtr' :: PKey k => k -> (Ptr EVP_PKEY -> IO a) -> IO a
withPKeyPtr' k f = do
  pk <- toPKey k
  withPKeyPtr pk f

unsafePKeyToPtr :: VaguePKey -> Ptr EVP_PKEY
unsafePKeyToPtr (VaguePKey pkey) = Unsafe.unsafeForeignPtrToPtr pkey

touchPKey :: VaguePKey -> IO ()
touchPKey (VaguePKey pkey) = touchForeignPtr pkey
