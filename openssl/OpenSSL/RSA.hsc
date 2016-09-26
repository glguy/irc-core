{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK prune             #-}
-- |An interface to RSA public key generator.
module OpenSSL.RSA
    ( -- * Type
      RSAKey(..)
    , RSAPubKey
    , RSAKeyPair
    , RSA -- private

      -- * Generating keypair
    , RSAGenKeyCallback
    , generateRSAKey
    , generateRSAKey'

      -- * Exploring keypair
    , rsaD
    , rsaP
    , rsaQ
    , rsaDMP1
    , rsaDMQ1
    , rsaIQMP
    , rsaCopyPublic
    , rsaKeyPairFinalize -- private
    )
    where
#include "HsOpenSSL.h"
import Control.Monad
import Data.Typeable
#if MIN_VERSION_base(4,5,0)
import Foreign.C.Types (CInt(..))
#else
import Foreign.C.Types (CInt)
#endif
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable(..))
import OpenSSL.BN
import OpenSSL.Utils
import System.IO.Unsafe (unsafePerformIO)

-- |@'RSAPubKey'@ is an opaque object that represents RSA public key.
newtype RSAPubKey  = RSAPubKey (ForeignPtr RSA)
    deriving Typeable

-- |@'RSAKeyPair'@ is an opaque object that represents RSA keypair.
newtype RSAKeyPair = RSAKeyPair (ForeignPtr RSA)
    deriving Typeable

-- RSAPubKey and RSAKeyPair are in fact the same type at the OpenSSL
-- level, but we want to treat them differently for type-safety.
data RSA

-- |@'RSAKey' a@ is either 'RSAPubKey' or 'RSAKeyPair'.
class RSAKey k where
    -- |@'rsaSize' key@ returns the length of key.
    rsaSize :: k -> Int
    rsaSize rsa
        = unsafePerformIO $
          withRSAPtr rsa $ \ rsaPtr ->
              fmap fromIntegral (_size rsaPtr)

    -- |@'rsaN' key@ returns the public modulus of the key.
    rsaN :: k -> Integer
    rsaN = peekI (#peek RSA, n)

    -- |@'rsaE' key@ returns the public exponent of the key.
    rsaE :: k -> Integer
    rsaE = peekI (#peek RSA, e)

    -- private
    withRSAPtr   :: k -> (Ptr RSA -> IO a) -> IO a
    peekRSAPtr   :: Ptr RSA -> IO (Maybe k)
    absorbRSAPtr :: Ptr RSA -> IO (Maybe k)


instance RSAKey RSAPubKey where
    withRSAPtr (RSAPubKey fp) = withForeignPtr fp
    peekRSAPtr rsaPtr         = _pubDup rsaPtr >>= absorbRSAPtr
    absorbRSAPtr rsaPtr       = fmap (Just . RSAPubKey) (newForeignPtr _free rsaPtr)


instance RSAKey RSAKeyPair where
    withRSAPtr (RSAKeyPair fp) = withForeignPtr fp
    peekRSAPtr rsaPtr
        = do hasP <- hasRSAPrivateKey rsaPtr
             if hasP then
                 _privDup rsaPtr >>= absorbRSAPtr
               else
                 return Nothing
    absorbRSAPtr rsaPtr
        = do hasP <- hasRSAPrivateKey rsaPtr
             if hasP then
                 fmap (Just . RSAKeyPair) (newForeignPtr _free rsaPtr)
               else
                 return Nothing


hasRSAPrivateKey :: Ptr RSA -> IO Bool
hasRSAPrivateKey rsaPtr
    = do d <- (#peek RSA, d) rsaPtr
         p <- (#peek RSA, p) rsaPtr
         q <- (#peek RSA, q) rsaPtr
         return (d /= nullPtr && p /= nullPtr && q /= nullPtr)
                                               


foreign import ccall unsafe "&RSA_free"
        _free :: FunPtr (Ptr RSA -> IO ())

foreign import ccall unsafe "RSAPublicKey_dup"
        _pubDup :: Ptr RSA -> IO (Ptr RSA)

foreign import ccall unsafe "RSAPrivateKey_dup"
        _privDup :: Ptr RSA -> IO (Ptr RSA)

foreign import ccall unsafe "RSA_size"
        _size :: Ptr RSA -> IO CInt

-- | Make a copy of the public parameters of the given key.
rsaCopyPublic :: RSAKey key => key -> IO RSAPubKey
rsaCopyPublic key = withRSAPtr key (fmap RSAPubKey . (newForeignPtr _free =<<) . _pubDup)

-- private
rsaKeyPairFinalize :: RSAKeyPair -> IO ()
rsaKeyPairFinalize (RSAKeyPair fp) = finalizeForeignPtr fp

{- generation --------------------------------------------------------------- -}

-- |@'RSAGenKeyCallback'@ represents a callback function to get
-- informed the progress of RSA key generation.
--
-- * @callback 0 i@ is called after generating the @i@-th potential
--   prime number.
--
-- * While the number is being tested for primality, @callback 1 j@ is
--   called after the @j@-th iteration (j = 0, 1, ...).
--
-- * When the @n@-th randomly generated prime is rejected as not
--   suitable for the key, @callback 2 n@ is called.
--
-- * When a random @p@ has been found with @p@-1 relatively prime to
--   @e@, it is called as @callback 3 0@.
--
-- * The process is then repeated for prime @q@ with @callback 3 1@.
type RSAGenKeyCallback = Int -> Int -> IO ()

type RSAGenKeyCallback' = Int -> Int -> Ptr () -> IO ()


foreign import ccall "wrapper"
        mkGenKeyCallback :: RSAGenKeyCallback' -> IO (FunPtr RSAGenKeyCallback')

foreign import ccall safe "RSA_generate_key"
        _generate_key :: CInt -> CInt -> FunPtr RSAGenKeyCallback' -> Ptr a -> IO (Ptr RSA)

-- |@'generateRSAKey'@ generates an RSA keypair.
generateRSAKey :: Int    -- ^ The number of bits of the public modulus
                         --   (i.e. key size). Key sizes with @n <
                         --   1024@ should be considered insecure.
               -> Int    -- ^ The public exponent. It is an odd
                         --   number, typically 3, 17 or 65537.
               -> Maybe RSAGenKeyCallback -- ^ A callback function.
               -> IO RSAKeyPair -- ^ The generated keypair.

generateRSAKey nbits e Nothing
    = do ptr <- _generate_key (fromIntegral nbits) (fromIntegral e) nullFunPtr nullPtr
         failIfNull_ ptr
         fmap RSAKeyPair (newForeignPtr _free ptr)

generateRSAKey nbits e (Just cb)
    = do cbPtr <- mkGenKeyCallback
                  $ \ arg1 arg2 _ -> cb arg1 arg2
         ptr   <- _generate_key (fromIntegral nbits) (fromIntegral e) cbPtr nullPtr
         freeHaskellFunPtr cbPtr
         failIfNull_ ptr
         fmap RSAKeyPair (newForeignPtr _free ptr)

-- |A simplified alternative to 'generateRSAKey'
generateRSAKey' :: Int   -- ^ The number of bits of the public modulus
                         --   (i.e. key size). Key sizes with @n <
                         --   1024@ should be considered insecure.
                -> Int   -- ^ The public exponent. It is an odd
                         --   number, typically 3, 17 or 65537.
                -> IO RSAKeyPair -- ^ The generated keypair.
generateRSAKey' nbits e
    = generateRSAKey nbits e Nothing


{- exploration -------------------------------------------------------------- -}

peekI :: RSAKey a => (Ptr RSA -> IO (Ptr BIGNUM)) -> a -> Integer
peekI peeker rsa
    = unsafePerformIO $
      withRSAPtr rsa $ \ rsaPtr ->
      do bn <- peeker rsaPtr
         when (bn == nullPtr) $ fail "peekI: got a nullPtr"
         peekBN (wrapBN bn)

peekMI :: RSAKey a => (Ptr RSA -> IO (Ptr BIGNUM)) -> a -> Maybe Integer
peekMI peeker rsa
    = unsafePerformIO $
      withRSAPtr rsa $ \ rsaPtr ->
      do bn <- peeker rsaPtr
         if bn == nullPtr then
             return Nothing
           else
             fmap Just (peekBN (wrapBN bn))

-- |@'rsaD' privKey@ returns the private exponent of the key.
rsaD :: RSAKeyPair -> Integer
rsaD = peekI (#peek RSA, d)

-- |@'rsaP' privkey@ returns the secret prime factor @p@ of the key.
rsaP :: RSAKeyPair -> Integer
rsaP = peekI (#peek RSA, p)

-- |@'rsaQ' privkey@ returns the secret prime factor @q@ of the key.
rsaQ :: RSAKeyPair -> Integer
rsaQ = peekI (#peek RSA, q)

-- |@'rsaDMP1' privkey@ returns @d mod (p-1)@ of the key.
rsaDMP1 :: RSAKeyPair -> Maybe Integer
rsaDMP1 = peekMI (#peek RSA, dmp1)

-- |@'rsaDMQ1' privkey@ returns @d mod (q-1)@ of the key.
rsaDMQ1 :: RSAKeyPair -> Maybe Integer
rsaDMQ1 = peekMI (#peek RSA, dmq1)

-- |@'rsaIQMP' privkey@ returns @q^-1 mod p@ of the key.
rsaIQMP :: RSAKeyPair -> Maybe Integer
rsaIQMP = peekMI (#peek RSA, iqmp)


{- instances ---------------------------------------------------------------- -}

instance Eq RSAPubKey where
    a == b
        = rsaN a == rsaN b &&
          rsaE a == rsaE b

instance Eq RSAKeyPair where
    a == b
        = rsaN a == rsaN b &&
          rsaE a == rsaE b &&
          rsaD a == rsaD b &&
          rsaP a == rsaP b &&
          rsaQ a == rsaQ b

instance Ord RSAPubKey where
    a `compare` b
        | rsaN a < rsaN b = LT
        | rsaN a > rsaN b = GT
        | rsaE a < rsaE b = LT
        | rsaE a > rsaE b = GT
        | otherwise       = EQ

instance Ord RSAKeyPair where
    a `compare` b
        | rsaN a < rsaN b = LT
        | rsaN a > rsaN b = GT
        | rsaE a < rsaE b = LT
        | rsaE a > rsaE b = GT
        | rsaD a < rsaD b = LT
        | rsaD a > rsaD b = GT
        | rsaP a < rsaP b = LT
        | rsaP a > rsaP b = GT
        | rsaQ a < rsaQ b = LT
        | rsaQ a > rsaQ b = GT
        | otherwise       = EQ

instance Show RSAPubKey where
    show a
        = concat [ "RSAPubKey {"
                 , "rsaN = ", show (rsaN a), ", "
                 , "rsaE = ", show (rsaE a)
                 , "}"
                 ] 

instance Show RSAKeyPair where
    show a
        = concat [ "RSAKeyPair {"
                 , "rsaN = ", show (rsaN a), ", "
                 , "rsaE = ", show (rsaE a), ", "
                 , "rsaD = ", show (rsaD a), ", "
                 , "rsaP = ", show (rsaP a), ", "
                 , "rsaQ = ", show (rsaQ a)
                 , "}"
                 ] 
