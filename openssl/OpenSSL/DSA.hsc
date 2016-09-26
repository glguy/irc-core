{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK prune             #-}
-- | The Digital Signature Algorithm (FIPS 186-2).
--   See <http://www.openssl.org/docs/crypto/dsa.html>
module OpenSSL.DSA
    ( -- * Type
      DSAKey(..)
    , DSAPubKey
    , DSAKeyPair
    , DSA -- private

      -- * Key and parameter generation
    , generateDSAParameters
    , generateDSAKey
    , generateDSAParametersAndKey

      -- * Signing and verification
    , signDigestedDataWithDSA
    , verifyDigestedDataWithDSA

      -- * Extracting fields of DSA objects
    , dsaPrivate
    , dsaPubKeyToTuple
    , dsaKeyPairToTuple
    , tupleToDSAPubKey
    , tupleToDSAKeyPair
    ) where
#include "HsOpenSSL.h"
import Control.Monad
import qualified Data.ByteString as BS
import Data.Typeable
import Foreign.C.String (CString)
#if MIN_VERSION_base(4,5,0)
import Foreign.C.Types (CChar(..), CInt(..))
#else
import Foreign.C.Types (CChar, CInt)
#endif
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import OpenSSL.BN
import OpenSSL.Utils
import System.IO.Unsafe (unsafePerformIO)

-- | The type of a DSA public key, includes parameters p, q, g and public.
newtype DSAPubKey = DSAPubKey (ForeignPtr DSA)
    deriving Typeable

-- | The type of a DSA keypair, includes parameters p, q, g, public and private.
newtype DSAKeyPair = DSAKeyPair (ForeignPtr DSA)
    deriving Typeable

-- DSAPubKey and DSAKeyPair are in fact the same type at the OpenSSL
-- level, but we want to treat them differently for type-safety.
data DSA

-- |@'DSAKey' a@ is either 'DSAPubKey' or 'DSAKeyPair'.
class DSAKey k where
    -- |Return the length of key.
    dsaSize :: k -> Int
    dsaSize dsa
        = unsafePerformIO $
          withDSAPtr dsa $ \ dsaPtr ->
              fmap fromIntegral (_size dsaPtr)

    -- |Return the public prime number of the key.
    dsaP :: k -> Integer
    dsaP = peekI (#peek DSA, p)

    -- |Return the public 160-bit subprime, @q | p - 1@ of the key.
    dsaQ :: k -> Integer
    dsaQ = peekI (#peek DSA, q)

    -- |Return the public generator of subgroup of the key.
    dsaG :: k -> Integer
    dsaG = peekI (#peek DSA, g)

    -- |Return the public key @y = g^x@.
    dsaPublic :: k -> Integer
    dsaPublic = peekI (#peek DSA, pub_key)

    -- private
    withDSAPtr   :: k -> (Ptr DSA -> IO a) -> IO a
    peekDSAPtr   :: Ptr DSA -> IO (Maybe k)
    absorbDSAPtr :: Ptr DSA -> IO (Maybe k)


instance DSAKey DSAPubKey where
    withDSAPtr (DSAPubKey fp) = withForeignPtr fp
    peekDSAPtr dsaPtr         = _pubDup dsaPtr >>= absorbDSAPtr
    absorbDSAPtr dsaPtr       = fmap (Just . DSAPubKey) (newForeignPtr _free dsaPtr)


instance DSAKey DSAKeyPair where
    withDSAPtr (DSAKeyPair fp) = withForeignPtr fp
    peekDSAPtr dsaPtr
        = do hasP <- hasDSAPrivateKey dsaPtr
             if hasP then
                 _privDup dsaPtr >>= absorbDSAPtr
               else
                 return Nothing
    absorbDSAPtr dsaPtr
        = do hasP <- hasDSAPrivateKey dsaPtr
             if hasP then
                 fmap (Just . DSAKeyPair) (newForeignPtr _free dsaPtr)
               else
                 return Nothing


hasDSAPrivateKey :: Ptr DSA -> IO Bool
hasDSAPrivateKey dsaPtr
    = fmap (/= nullPtr) ((#peek DSA, priv_key) dsaPtr)


foreign import ccall unsafe "&DSA_free"
        _free :: FunPtr (Ptr DSA -> IO ())

foreign import ccall unsafe "DSA_free"
        dsa_free :: Ptr DSA -> IO ()

foreign import ccall unsafe "BN_free"
        _bn_free :: Ptr BIGNUM -> IO ()

foreign import ccall unsafe "DSA_new"
        _dsa_new :: IO (Ptr DSA)

foreign import ccall unsafe "DSA_generate_key"
        _dsa_generate_key :: Ptr DSA -> IO ()

foreign import ccall unsafe "HsOpenSSL_dsa_sign"
        _dsa_sign :: Ptr DSA -> CString -> CInt -> Ptr (Ptr BIGNUM) -> Ptr (Ptr BIGNUM) -> IO CInt

foreign import ccall unsafe "HsOpenSSL_dsa_verify"
        _dsa_verify :: Ptr DSA -> CString -> CInt -> Ptr BIGNUM -> Ptr BIGNUM -> IO CInt

foreign import ccall safe "DSA_generate_parameters"
        _generate_params :: CInt -> Ptr CChar -> CInt -> Ptr CInt -> Ptr CInt -> Ptr () -> Ptr () -> IO (Ptr DSA)

foreign import ccall unsafe "HsOpenSSL_DSAPublicKey_dup"
        _pubDup :: Ptr DSA -> IO (Ptr DSA)

foreign import ccall unsafe "HsOpenSSL_DSAPrivateKey_dup"
        _privDup :: Ptr DSA -> IO (Ptr DSA)

foreign import ccall unsafe "DSA_size"
        _size :: Ptr DSA -> IO CInt

peekI :: DSAKey k => (Ptr DSA -> IO (Ptr BIGNUM)) -> k -> Integer
peekI peeker dsa
    = unsafePerformIO $
      withDSAPtr dsa $ \ dsaPtr ->
          do bn <- peeker dsaPtr
             when (bn == nullPtr) $ fail "peekI: got a nullPtr"
             peekBN (wrapBN bn)

-- | Generate DSA parameters (*not* a key, but required for a key). This is a
--   compute intensive operation. See FIPS 186-2, app 2. This agrees with the
--   test vectors given in FIP 186-2, app 5
generateDSAParameters :: Int  -- ^ The number of bits in the generated prime: 512 <= x <= 1024
                      -> Maybe BS.ByteString  -- ^ optional seed, its length must be 20 bytes
                      -> IO (Int, Int, Integer, Integer, Integer)  -- ^ (iteration count, generator count, p, q, g)
generateDSAParameters nbits mseed = do
  when (nbits < 512 || nbits > 1024) $ fail "Invalid DSA bit size"
  alloca (\i1 ->
    alloca (\i2 ->
      (\x -> case mseed of
                  Nothing -> x (nullPtr, 0)
                  Just seed -> BS.useAsCStringLen seed x) (\(seedptr, seedlen) -> do
        ptr <- _generate_params (fromIntegral nbits) seedptr (fromIntegral seedlen) i1 i2 nullPtr nullPtr
        failIfNull_ ptr
        itcount <- peek i1
        gencount <- peek i2
        p <- (#peek DSA, p) ptr >>= peekBN . wrapBN
        q <- (#peek DSA, q) ptr >>= peekBN . wrapBN
        g <- (#peek DSA, g) ptr >>= peekBN . wrapBN
        dsa_free ptr
        return (fromIntegral itcount, fromIntegral gencount, p, q, g))))

{-
-- | This function just runs the example DSA generation, as given in FIP 186-2,
--   app 5. The return values should be:
--   (105,
--    "8df2a494492276aa3d25759bb06869cbeac0d83afb8d0cf7cbb8324f0d7882e5d0762fc5b7210
--     eafc2e9adac32ab7aac49693dfbf83724c2ec0736ee31c80291",
--     "c773218c737ec8ee993b4f2ded30f48edace915f",
--     "626d027839ea0a13413163a55b4cb500299d5522956cefcb3bff10f399ce2c2e71cb9de5fa24
--      babf58e5b79521925c9cc42e9f6f464b088cc572af53e6d78802"), as given at the bottom of
--    page 21
test_generateParameters = do
  let seed = BS.pack [0xd5, 0x01, 0x4e, 0x4b,
                      0x60, 0xef, 0x2b, 0xa8,
                      0xb6, 0x21, 0x1b, 0x40,
                      0x62, 0xba, 0x32, 0x24,
                      0xe0, 0x42, 0x7d, 0xd3]
  (a, b, p, q, g) <- generateParameters 512 $ Just seed
  return (a, toHex p, toHex q, g)
-}

-- | Generate a new DSA keypair, given valid parameters
generateDSAKey :: Integer  -- ^ p
               -> Integer  -- ^ q
               -> Integer  -- ^ g
               -> IO DSAKeyPair
generateDSAKey p q g = do
  ptr <- _dsa_new
  fmap unwrapBN (newBN p) >>= (#poke DSA, p) ptr
  fmap unwrapBN (newBN q) >>= (#poke DSA, q) ptr
  fmap unwrapBN (newBN g) >>= (#poke DSA, g) ptr
  _dsa_generate_key ptr
  fmap DSAKeyPair (newForeignPtr _free ptr)

-- |Return the private key @x@.
dsaPrivate :: DSAKeyPair -> Integer
dsaPrivate = peekI (#peek DSA, priv_key)

-- | Convert a DSAPubKey object to a tuple of its members in the
--   order p, q, g, and public.
dsaPubKeyToTuple :: DSAKeyPair -> (Integer, Integer, Integer, Integer)
dsaPubKeyToTuple dsa
    = let p   = peekI (#peek DSA, p) dsa
          q   = peekI (#peek DSA, q) dsa
          g   = peekI (#peek DSA, g) dsa
          pub = peekI (#peek DSA, pub_key) dsa
      in
        (p, q, g, pub)

-- | Convert a DSAKeyPair object to a tuple of its members in the
--   order p, q, g, public and private.
dsaKeyPairToTuple :: DSAKeyPair -> (Integer, Integer, Integer, Integer, Integer)
dsaKeyPairToTuple dsa
    = let p   = peekI (#peek DSA, p) dsa
          q   = peekI (#peek DSA, q) dsa
          g   = peekI (#peek DSA, g) dsa
          pub = peekI (#peek DSA, pub_key ) dsa
          pri = peekI (#peek DSA, priv_key) dsa
      in
        (p, q, g, pub, pri)

-- | Convert a tuple of members (in the same format as from
--   'dsaPubKeyToTuple') into a DSAPubKey object
tupleToDSAPubKey :: (Integer, Integer, Integer, Integer) -> DSAPubKey
tupleToDSAPubKey (p, q, g, pub) = unsafePerformIO $ do
  ptr <- _dsa_new
  fmap unwrapBN (newBN p  ) >>= (#poke DSA, p) ptr
  fmap unwrapBN (newBN q  ) >>= (#poke DSA, q) ptr
  fmap unwrapBN (newBN g  ) >>= (#poke DSA, g) ptr
  fmap unwrapBN (newBN pub) >>= (#poke DSA, pub_key) ptr
  (#poke DSA, priv_key) ptr nullPtr
  fmap DSAPubKey (newForeignPtr _free ptr)

-- | Convert a tuple of members (in the same format as from
--   'dsaPubKeyToTuple') into a DSAPubKey object
tupleToDSAKeyPair :: (Integer, Integer, Integer, Integer, Integer) -> DSAKeyPair
tupleToDSAKeyPair (p, q, g, pub, pri) = unsafePerformIO $ do
  ptr <- _dsa_new
  fmap unwrapBN (newBN p  ) >>= (#poke DSA, p) ptr
  fmap unwrapBN (newBN q  ) >>= (#poke DSA, q) ptr
  fmap unwrapBN (newBN g  ) >>= (#poke DSA, g) ptr
  fmap unwrapBN (newBN pub) >>= (#poke DSA, pub_key ) ptr
  fmap unwrapBN (newBN pri) >>= (#poke DSA, priv_key) ptr
  fmap DSAKeyPair (newForeignPtr _free ptr)

-- | A utility function to generate both the parameters and the key pair at the
--   same time. Saves serialising and deserialising the parameters too
generateDSAParametersAndKey :: Int  -- ^ The number of bits in the generated prime: 512 <= x <= 1024
                            -> Maybe BS.ByteString  -- ^ optional seed, its length must be 20 bytes
                            -> IO DSAKeyPair
generateDSAParametersAndKey nbits mseed =
  (\x -> case mseed of
              Nothing -> x (nullPtr, 0)
              Just seed -> BS.useAsCStringLen seed x) (\(seedptr, seedlen) -> do
    ptr <- _generate_params (fromIntegral nbits) seedptr (fromIntegral seedlen) nullPtr nullPtr nullPtr nullPtr
    failIfNull_ ptr
    _dsa_generate_key ptr
    fmap DSAKeyPair (newForeignPtr _free ptr))

-- | Sign pre-digested data. The DSA specs call for SHA1 to be used so, if you
--   use anything else, YMMV. Returns a pair of Integers which, together, are
--   the signature
signDigestedDataWithDSA :: DSAKeyPair -> BS.ByteString -> IO (Integer, Integer)
signDigestedDataWithDSA dsa bytes =
  BS.useAsCStringLen bytes (\(ptr, len) ->
    alloca (\rptr ->
      alloca (\sptr ->
        withDSAPtr dsa (\dsaptr -> do
          _dsa_sign dsaptr ptr (fromIntegral len) rptr sptr >>= failIf_ (== 0)
          r <- peek rptr >>= peekBN . wrapBN
          peek rptr >>= _bn_free
          s <- peek sptr >>= peekBN . wrapBN
          peek sptr >>= _bn_free
          return (r, s)))))

-- | Verify pre-digested data given a signature.
verifyDigestedDataWithDSA :: DSAKey k => k -> BS.ByteString -> (Integer, Integer) -> IO Bool
verifyDigestedDataWithDSA dsa bytes (r, s) =
  BS.useAsCStringLen bytes (\(ptr, len) ->
    withBN r (\bnR ->
      withBN s (\bnS ->
        withDSAPtr dsa (\dsaptr ->
          fmap (== 1)
               (_dsa_verify dsaptr ptr (fromIntegral len) (unwrapBN bnR) (unwrapBN bnS))))))


instance Eq DSAPubKey where
    a == b
        = dsaP      a == dsaP      b &&
          dsaQ      a == dsaQ      b &&
          dsaG      a == dsaG      b &&
          dsaPublic a == dsaPublic b

instance Eq DSAKeyPair where
    a == b
        = dsaP       a == dsaP       b &&
          dsaQ       a == dsaQ       b &&
          dsaG       a == dsaG       b &&
          dsaPublic  a == dsaPublic  b &&
          dsaPrivate a == dsaPrivate b

instance Ord DSAPubKey where
    a `compare` b
        | dsaP      a < dsaP      b = LT
        | dsaP      a > dsaP      b = GT
        | dsaQ      a < dsaQ      b = LT
        | dsaQ      a > dsaQ      b = GT
        | dsaG      a < dsaG      b = LT
        | dsaG      a > dsaG      b = GT
        | dsaPublic a < dsaPublic b = LT
        | dsaPublic a > dsaPublic b = GT
        | otherwise                 = EQ

instance Ord DSAKeyPair where
    a `compare` b
        | dsaP       a < dsaP       b = LT
        | dsaP       a > dsaP       b = GT
        | dsaQ       a < dsaQ       b = LT
        | dsaQ       a > dsaQ       b = GT
        | dsaG       a < dsaG       b = LT
        | dsaG       a > dsaG       b = GT
        | dsaPublic  a < dsaPublic  b = LT
        | dsaPublic  a > dsaPublic  b = GT
        | dsaPrivate a < dsaPrivate b = LT
        | dsaPrivate a > dsaPrivate b = GT
        | otherwise                   = EQ

instance Show DSAPubKey where
    show a
        = concat [ "DSAPubKey {"
                 , "dsaP = ", show (dsaP a), ", "
                 , "dsaQ = ", show (dsaQ a), ", "
                 , "dsaG = ", show (dsaG a), ", "
                 , "dsaPublic = ", show (dsaPublic a)
                 , "}"
                 ]

instance Show DSAKeyPair where
    show a
        = concat [ "DSAPubKey {"
                 , "dsaP = ", show (dsaP a), ", "
                 , "dsaQ = ", show (dsaQ a), ", "
                 , "dsaG = ", show (dsaG a), ", "
                 , "dsaPublic = ", show (dsaPublic a), ", "
                 , "dsaPrivate = ", show (dsaPrivate a)
                 , "}"
                 ]