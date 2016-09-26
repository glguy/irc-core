{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | Diffie-Hellman key exchange
module OpenSSL.DH
    ( DHP
    , DH
    , DHGen(..)
    , genDHParams
    , getDHLength
    , checkDHParams
    , genDH
    , getDHParams
    , getDHPublicKey
    , computeDHKey
    )
    where
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import Control.Applicative ((<$>))
import Foreign.Ptr (Ptr, nullPtr)
#if MIN_VERSION_base(4,5,0)
import Foreign.C.Types (CInt(..))
#else
import Foreign.C.Types (CInt)
#endif
import Foreign.Marshal.Alloc (alloca)
import OpenSSL.BN
import OpenSSL.DH.Internal
import OpenSSL.Utils

data DHGen = DHGen2
           | DHGen5
           deriving (Eq, Ord, Show)

-- | @'genDHParams' gen n@ generates @n@-bit long DH parameters.
genDHParams :: DHGen -> Int -> IO DHP
genDHParams gen len = do
    _DH_generate_parameters (fromIntegral len) gen' nullPtr nullPtr
      >>= failIfNull
      >>= wrapDHPPtr
  where gen' = case gen of
                 DHGen2 -> 2
                 DHGen5 -> 5

-- | Get DH parameters length (in bits).
getDHLength :: DHP -> IO Int
getDHLength dh = fromIntegral <$> withDHPPtr dh _DH_length

-- | Check that DH parameters are coherent.
checkDHParams :: DHP -> IO Bool
checkDHParams dh = alloca $ \pErr ->
                     withDHPPtr dh $ \dhPtr -> _DH_check dhPtr pErr

-- | The first step of a key exchange. Public and private keys are generated.
genDH :: DHP -> IO DH
genDH dh = do
  dh' <- withDHPPtr dh _DH_dup >>= failIfNull >>= wrapDHPPtr
  withDHPPtr dh' _DH_generate_key >>= failIf_ (/= 1)
  return $ asDH dh'

-- | Get parameters of a key exchange.
getDHParams :: DH -> DHP
getDHParams = asDHP

-- | Get the public key.
getDHPublicKey :: DH -> IO Integer
getDHPublicKey dh =
  withDHPtr dh $ \dhPtr -> do
    pKey <- _DH_get_pub_key dhPtr
    bnToInteger (wrapBN pKey)

-- | Compute the shared key using the other party's public key.
computeDHKey :: DH -> Integer -> IO ByteString
computeDHKey dh pubKey =
  withDHPtr dh $ \dhPtr ->
    withBN pubKey $ \bn -> do
      size <- fromIntegral <$> _DH_size dhPtr
      BS.createAndTrim size $ \bsPtr ->
        fromIntegral <$> _DH_compute_key bsPtr (unwrapBN bn) dhPtr
          >>= failIf (< 0)

foreign import ccall "DH_generate_parameters"
  _DH_generate_parameters :: CInt -> CInt -> Ptr () -> Ptr () -> IO (Ptr DH_)
foreign import ccall "DH_generate_key"
  _DH_generate_key :: Ptr DH_ -> IO CInt
foreign import ccall "DH_compute_key"
  _DH_compute_key :: Ptr Word8 -> Ptr BIGNUM -> Ptr DH_ -> IO CInt
foreign import ccall "DH_check"
  _DH_check :: Ptr DH_ -> Ptr CInt -> IO Bool
foreign import ccall unsafe "DH_size"
  _DH_size :: Ptr DH_ -> IO CInt
foreign import ccall unsafe "HsOpenSSL_DHparams_dup"
  _DH_dup :: Ptr DH_ -> IO (Ptr DH_)
foreign import ccall unsafe "HsOpenSSL_DH_get_pub_key"
  _DH_get_pub_key :: Ptr DH_ -> IO (Ptr BIGNUM)
foreign import ccall unsafe "HsOpenSSL_DH_length"
  _DH_length :: Ptr DH_ -> IO CInt

