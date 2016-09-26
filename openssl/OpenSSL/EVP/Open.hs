{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |Asymmetric cipher decryption using encrypted symmetric key. This
-- is an opposite of "OpenSSL.EVP.Seal".
module OpenSSL.EVP.Open
    ( open
    , openBS
    , openLBS
    )
    where
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Unsafe as B8
import Foreign.C.String (CString)
#if MIN_VERSION_base(4,5,0)
import Foreign.C.Types (CChar(..), CInt(..))
#else
import Foreign.C.Types (CChar, CInt)
#endif
import Foreign.Ptr (Ptr)
import OpenSSL.EVP.Cipher hiding (cipher)
import OpenSSL.EVP.PKey
import OpenSSL.EVP.Internal
import OpenSSL.Utils
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "EVP_OpenInit"
        _OpenInit :: Ptr EVP_CIPHER_CTX
                  -> Cipher
                  -> Ptr CChar
                  -> CInt
                  -> CString
                  -> Ptr EVP_PKEY
                  -> IO CInt


openInit :: KeyPair key =>
            Cipher
         -> B8.ByteString
         -> B8.ByteString
         -> key
         -> IO CipherCtx
openInit cipher encKey iv pkey
    = do ctx <- newCipherCtx
         withCipherCtxPtr ctx $ \ ctxPtr ->
             B8.unsafeUseAsCStringLen encKey $ \ (encKeyPtr, encKeyLen) ->
                 B8.unsafeUseAsCString iv $ \ ivPtr ->
                     withPKeyPtr' pkey $ \ pkeyPtr ->
                         _OpenInit ctxPtr cipher encKeyPtr (fromIntegral encKeyLen) ivPtr pkeyPtr
                              >>= failIf_ (== 0)
         return ctx

-- |@'open'@ lazilly decrypts a stream of data. The input string
-- doesn't necessarily have to be finite.
open :: KeyPair key =>
        Cipher -- ^ symmetric cipher algorithm to use
     -> String -- ^ encrypted symmetric key to decrypt the input string
     -> String -- ^ IV
     -> key    -- ^ private key to decrypt the symmetric key
     -> String -- ^ input string to decrypt
     -> String -- ^ decrypted string
{-# DEPRECATED open "Use openBS or openLBS instead." #-}
open cipher encKey iv pkey input
    = L8.unpack $ openLBS cipher (B8.pack encKey) (B8.pack iv) pkey (L8.pack input)

-- |@'openBS'@ decrypts a chunk of data.
openBS :: KeyPair key =>
          Cipher        -- ^ symmetric cipher algorithm to use
       -> B8.ByteString -- ^ encrypted symmetric key to decrypt the input string
       -> B8.ByteString -- ^ IV
       -> key           -- ^ private key to decrypt the symmetric key
       -> B8.ByteString -- ^ input string to decrypt
       -> B8.ByteString -- ^ decrypted string
openBS cipher encKey iv pkey input
    = unsafePerformIO $
      do ctx <- openInit cipher encKey iv pkey
         cipherStrictly ctx input

-- |@'openLBS'@ lazilly decrypts a stream of data. The input string
-- doesn't necessarily have to be finite.
openLBS :: KeyPair key =>
           Cipher        -- ^ symmetric cipher algorithm to use
        -> B8.ByteString -- ^ encrypted symmetric key to decrypt the input string
        -> B8.ByteString -- ^ IV
        -> key           -- ^ private key to decrypt the symmetric key
        -> L8.ByteString -- ^ input string to decrypt
        -> L8.ByteString -- ^ decrypted string
openLBS cipher encKey iv pkey input
    = unsafePerformIO $
      do ctx <- openInit cipher encKey iv pkey
         cipherLazily ctx input
