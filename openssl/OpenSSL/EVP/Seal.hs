{-# LANGUAGE ForeignFunctionInterface #-}
-- |Asymmetric cipher decryption using encrypted symmetric key. This
-- is an opposite of "OpenSSL.EVP.Open".
module OpenSSL.EVP.Seal
    ( seal
    , sealBS
    , sealLBS
    )
    where
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Foreign
import           Foreign.C
import           OpenSSL.EVP.Cipher hiding (cipher)
import           OpenSSL.EVP.PKey
import           OpenSSL.EVP.Internal
import           OpenSSL.Utils


foreign import ccall unsafe "EVP_SealInit"
        _SealInit :: Ptr EVP_CIPHER_CTX
                  -> Cipher
                  -> Ptr (Ptr CChar)
                  -> Ptr CInt
                  -> CString
                  -> Ptr (Ptr EVP_PKEY)
                  -> CInt
                  -> IO CInt


sealInit :: Cipher
         -> [SomePublicKey]
         -> IO (CipherCtx, [B8.ByteString], B8.ByteString)

sealInit _ []
    = fail "sealInit: at least one public key is required"

sealInit cipher pubKeys
    = do ctx <- newCipherCtx

         -- Allocate a list of buffers to write encrypted symmetric
         -- keys. Each keys will be at most pkeySize bytes long.
         encKeyBufs <- mapM mallocEncKeyBuf pubKeys

         -- encKeyBufs is [Ptr a] but we want Ptr (Ptr CChar).
         encKeyBufsPtr <- newArray encKeyBufs

         -- Allocate a buffer to write lengths of each encrypted
         -- symmetric keys.
         encKeyBufsLenPtr <- mallocArray nKeys

         -- Allocate a buffer to write IV.
         ivPtr <- mallocArray (cipherIvLength cipher)

         -- Create Ptr (Ptr EVP_PKEY) from [PKey]. Don't forget to
         -- apply touchForeignPtr to each PKey's later.
         pkeys      <- mapM toPKey pubKeys
         pubKeysPtr <- newArray $ map unsafePKeyToPtr pkeys

         -- Prepare an IO action to free buffers we allocated above.
         let cleanup = do mapM_ free encKeyBufs
                          free encKeyBufsPtr
                          free encKeyBufsLenPtr
                          free ivPtr
                          free pubKeysPtr
                          mapM_ touchPKey pkeys

         -- Call EVP_SealInit finally.
         ret <- withCipherCtxPtr ctx $ \ ctxPtr ->
                _SealInit ctxPtr cipher encKeyBufsPtr encKeyBufsLenPtr ivPtr pubKeysPtr (fromIntegral nKeys)

         if ret == 0 then
             cleanup >> raiseOpenSSLError
           else
             do encKeysLen <- peekArray nKeys encKeyBufsLenPtr
                encKeys    <- mapM B8.packCStringLen $ zip encKeyBufs (fromIntegral `fmap` encKeysLen)
                iv         <- B8.packCStringLen (ivPtr, cipherIvLength cipher)
                cleanup
                return (ctx, encKeys, iv)
    where
      nKeys :: Int
      nKeys = length pubKeys

      mallocEncKeyBuf :: (PKey k, Storable a) => k -> IO (Ptr a)
      mallocEncKeyBuf = mallocArray . pkeySize

-- |@'seal'@ lazilly encrypts a stream of data. The input string
-- doesn't necessarily have to be finite.
seal :: Cipher          -- ^ symmetric cipher algorithm to use
     -> [SomePublicKey] -- ^ A list of public keys to encrypt a
                        --   symmetric key. At least one public key
                        --   must be supplied. If two or more keys are
                        --   given, the symmetric key are encrypted by
                        --   each public keys so that any of the
                        --   corresponding private keys can decrypt
                        --   the message.
     -> String          -- ^ input string to encrypt
     -> IO ( String
           , [String]
           , String
           ) -- ^ (encrypted string, list of encrypted asymmetric
             -- keys, IV)
{-# DEPRECATED seal "Use sealBS or sealLBS instead." #-}
seal cipher pubKeys input
    = do (output, encKeys, iv) <- sealLBS cipher pubKeys $ L8.pack input
         return ( L8.unpack output
                , B8.unpack `fmap` encKeys
                , B8.unpack iv
                )

-- |@'sealBS'@ strictly encrypts a chunk of data.
sealBS :: Cipher          -- ^ symmetric cipher algorithm to use
       -> [SomePublicKey] -- ^ list of public keys to encrypt a
                          --   symmetric key
       -> B8.ByteString   -- ^ input string to encrypt
       -> IO ( B8.ByteString
             , [B8.ByteString]
             , B8.ByteString
             ) -- ^ (encrypted string, list of encrypted asymmetric
               -- keys, IV)
sealBS cipher pubKeys input
    = do (ctx, encKeys, iv) <- sealInit cipher pubKeys
         output             <- cipherStrictly ctx input
         return (output, encKeys, iv)

-- |@'sealLBS'@ lazilly encrypts a stream of data. The input string
-- doesn't necessarily have to be finite.
sealLBS :: Cipher          -- ^ symmetric cipher algorithm to use
        -> [SomePublicKey] -- ^ list of public keys to encrypt a
                           --   symmetric key
        -> L8.ByteString   -- ^ input string to encrypt
        -> IO ( L8.ByteString
              , [B8.ByteString]
              , B8.ByteString
              ) -- ^ (encrypted string, list of encrypted asymmetric
                -- keys, IV)
sealLBS cipher pubKeys input
    = do (ctx, encKeys, iv) <- sealInit cipher pubKeys
         output             <- cipherLazily ctx input
         return (output, encKeys, iv)
