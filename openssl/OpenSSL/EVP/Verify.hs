{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |Message verification using asymmetric cipher and message digest
-- algorithm. This is an opposite of "OpenSSL.EVP.Sign".
module OpenSSL.EVP.Verify
    ( VerifyStatus(..)
    , verify
    , verifyBS
    , verifyLBS
    )
    where
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Unsafe as B8
import           Data.Typeable
import           Foreign
import           Foreign.C
import           OpenSSL.EVP.Digest
import           OpenSSL.EVP.PKey
import           OpenSSL.EVP.Internal
import           OpenSSL.Utils

-- |@'VerifyStatus'@ represents a result of verification.
data VerifyStatus = VerifySuccess
                  | VerifyFailure
                    deriving (Show, Eq, Typeable)


foreign import ccall unsafe "EVP_VerifyFinal"
        _VerifyFinal :: Ptr EVP_MD_CTX -> Ptr CChar -> CUInt -> Ptr EVP_PKEY -> IO CInt


verifyFinalBS :: PublicKey k =>
                 DigestCtx
              -> B8.ByteString
              -> k
              -> IO VerifyStatus
verifyFinalBS ctx sig k
    = withDigestCtxPtr ctx $ \ ctxPtr ->
      B8.unsafeUseAsCStringLen sig $ \ (buf, len) ->
      withPKeyPtr' k $ \ pkeyPtr ->
      _VerifyFinal ctxPtr buf (fromIntegral len) pkeyPtr >>= interpret
    where
      interpret :: CInt -> IO VerifyStatus
      interpret 1 = return VerifySuccess
      interpret 0 = return VerifyFailure
      interpret _ = raiseOpenSSLError

-- |@'verify'@ verifies a signature and a stream of data. The string
-- must not contain any letters which aren't in the range of U+0000 -
-- U+00FF.
verify :: PublicKey key =>
          Digest          -- ^ message digest algorithm to use
       -> String          -- ^ message signature
       -> key             -- ^ public key to verify the signature
       -> String          -- ^ input string to verify
       -> IO VerifyStatus -- ^ the result of verification
{-# DEPRECATED verify "Use verifyBS or verifyLBS instead." #-}
verify md sig pkey input
    = verifyLBS md (B8.pack sig) pkey (L8.pack input)

-- |@'verifyBS'@ verifies a signature and a chunk of data.
verifyBS :: PublicKey key =>
            Digest          -- ^ message digest algorithm to use
         -> B8.ByteString   -- ^ message signature
         -> key             -- ^ public key to verify the signature
         -> B8.ByteString   -- ^ input string to verify
         -> IO VerifyStatus -- ^ the result of verification
verifyBS md sig pkey input
    = do ctx <- digestStrictly md input
         verifyFinalBS ctx sig pkey

-- |@'verifyLBS'@ verifies a signature of a stream of data.
verifyLBS :: PublicKey key =>
             Digest          -- ^ message digest algorithm to use
          -> B8.ByteString   -- ^ message signature
          -> key             -- ^ public key to verify the signature
          -> L8.ByteString   -- ^ input string to verify
          -> IO VerifyStatus -- ^ the result of verification
verifyLBS md sig pkey input
    = do ctx <- digestLazily md input
         verifyFinalBS ctx sig pkey
