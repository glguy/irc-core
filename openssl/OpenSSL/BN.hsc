#if defined(FAST_BIGNUM)
{-# LANGUAGE BangPatterns             #-}
#endif
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#if defined(FAST_BIGNUM)
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE UnliftedFFITypes         #-}
#endif
{-# OPTIONS_HADDOCK prune             #-}
-- |BN - multiprecision integer arithmetics
module OpenSSL.BN
    ( -- * Type
      BigNum
    , BIGNUM

      -- * Allocation
    , allocaBN
    , withBN

    , newBN
    , wrapBN -- private
    , unwrapBN -- private

      -- * Conversion from\/to Integer
    , peekBN
    , integerToBN
    , bnToInteger
    , integerToMPI
    , mpiToInteger

      -- * Computation
    , modexp

      -- * Random number generation
    , randIntegerUptoNMinusOneSuchThat
    , prandIntegerUptoNMinusOneSuchThat
    , randIntegerZeroToNMinusOne
    , prandIntegerZeroToNMinusOne
    , randIntegerOneToNMinusOne
    , prandIntegerOneToNMinusOne
    )
    where
#include "HsOpenSSL.h"
import           Control.Exception hiding (try)
import qualified Data.ByteString as BS
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable
import           OpenSSL.Utils
import           System.IO.Unsafe

#if defined(FAST_BIGNUM)
import           Foreign.C.Types
import           GHC.Base
#  if MIN_VERSION_integer_gmp(0,2,0)
import           GHC.Integer.GMP.Internals
#  else
import           GHC.Num
import           GHC.Prim
import           GHC.Integer.Internals
import           GHC.IOBase (IO(..))
#  endif
#else
import           Control.Monad
import           Foreign.C
#endif

-- |'BigNum' is an opaque object representing a big number.
newtype BigNum = BigNum (Ptr BIGNUM)
data BIGNUM


foreign import ccall unsafe "BN_new"
        _new :: IO (Ptr BIGNUM)

foreign import ccall unsafe "BN_free"
        _free :: Ptr BIGNUM -> IO ()

-- |@'allocaBN' f@ allocates a 'BigNum' and computes @f@. Then it
-- frees the 'BigNum'.
allocaBN :: (BigNum -> IO a) -> IO a
allocaBN m
    = bracket _new _free (m . wrapBN)


unwrapBN :: BigNum -> Ptr BIGNUM
unwrapBN (BigNum p) = p


wrapBN :: Ptr BIGNUM -> BigNum
wrapBN = BigNum


#if !defined(FAST_BIGNUM)

{- slow, safe functions ----------------------------------------------------- -}

foreign import ccall unsafe "BN_bn2dec"
        _bn2dec :: Ptr BIGNUM -> IO CString

foreign import ccall unsafe "BN_dec2bn"
        _dec2bn :: Ptr (Ptr BIGNUM) -> CString -> IO CInt

foreign import ccall unsafe "HsOpenSSL_OPENSSL_free"
        _openssl_free :: Ptr a -> IO ()

-- |Convert a BIGNUM to an 'Integer'.
bnToInteger :: BigNum -> IO Integer
bnToInteger bn
    = bracket (do strPtr <- _bn2dec (unwrapBN bn)
                  when (strPtr == nullPtr) $ fail "BN_bn2dec failed"
                  return strPtr)
              _openssl_free
              ((read `fmap`) . peekCString)

-- |Return a new, alloced BIGNUM.
integerToBN :: Integer -> IO BigNum
integerToBN i = do
  withCString (show i) (\str -> do
    alloca (\bnptr -> do
      poke bnptr nullPtr
      _ <- _dec2bn bnptr str >>= failIf (== 0)
      wrapBN `fmap` peek bnptr))

#else

{- fast, dangerous functions ------------------------------------------------ -}

-- Both BN (the OpenSSL library) and GMP (used by GHC) use the same internal
-- representation for numbers: an array of words, least-significant first. Thus
-- we can move from Integer's to BIGNUMs very quickly: by copying in the worst
-- case and by just alloca'ing and pointing into the Integer in the fast case.
-- Note that, in the fast case, it's very important that any foreign function
-- calls be "unsafe", that is, they don't call back into Haskell. Otherwise the
-- GC could do nasty things to the data which we thought that we had a pointer
-- to

foreign import ccall unsafe "memcpy"
        _copy_in :: ByteArray## -> Ptr () -> CSize -> IO (Ptr ())

foreign import ccall unsafe "memcpy"
        _copy_out :: Ptr () -> ByteArray## -> CSize -> IO (Ptr ())

-- These are taken from Data.Binary's disabled fast Integer support
data ByteArray = BA  !ByteArray##
data MBA       = MBA !(MutableByteArray## RealWorld)

newByteArray :: Int## -> IO MBA
newByteArray sz = IO $ \s ->
  case newByteArray## sz s of { (## s', arr ##) ->
  (## s', MBA arr ##) }

freezeByteArray :: MutableByteArray## RealWorld -> IO ByteArray
freezeByteArray arr = IO $ \s ->
  case unsafeFreezeByteArray## arr s of { (## s', arr' ##) ->
  (## s', BA arr' ##) }

-- | Convert a BIGNUM to an Integer
bnToInteger :: BigNum -> IO Integer
bnToInteger bn = do
  nlimbs <- (#peek BIGNUM, top) (unwrapBN bn) :: IO CInt
  case nlimbs of
    0 -> return 0
    1 -> do (I## i) <- (#peek BIGNUM, d) (unwrapBN bn) >>= peek
            negative <- (#peek BIGNUM, neg) (unwrapBN bn) :: IO CInt
            if negative == 0
               then return $ S## i
               else return $ 0 - (S## i)
    _ -> do
      let !(I## nlimbsi) = fromIntegral nlimbs
          !(I## limbsize) = (#size unsigned long)
      (MBA arr) <- newByteArray (nlimbsi *## limbsize)
      (BA ba) <- freezeByteArray arr
      limbs <- (#peek BIGNUM, d) (unwrapBN bn)
      _ <- _copy_in ba limbs $ fromIntegral $ nlimbs * (#size unsigned long)
      negative <- (#peek BIGNUM, neg) (unwrapBN bn) :: IO CInt
      if negative == 0
         then return $ J## nlimbsi ba
         else return $ 0 - (J## nlimbsi ba)

-- | This is a GHC specific, fast conversion between Integers and OpenSSL
--   bignums. It returns a malloced BigNum.
integerToBN :: Integer -> IO BigNum
integerToBN (S## 0##) = do
  bnptr <- mallocBytes (#size BIGNUM)
  (#poke BIGNUM, d) bnptr nullPtr
  -- This is needed to give GHC enough type information
  let one :: CInt
      one = 1
      zero :: CInt
      zero = 0
  (#poke BIGNUM, flags) bnptr one
  (#poke BIGNUM, top) bnptr zero
  (#poke BIGNUM, dmax) bnptr zero
  (#poke BIGNUM, neg) bnptr zero
  return (wrapBN bnptr)

integerToBN (S## v) = do
  bnptr <- mallocBytes (#size BIGNUM)
  limbs <- malloc :: IO (Ptr CULong)
  poke limbs $ fromIntegral $ abs $ I## v
  (#poke BIGNUM, d) bnptr limbs
  -- This is needed to give GHC enough type information since #poke just
  -- uses an offset
  let one :: CInt
      one = 1
  (#poke BIGNUM, flags) bnptr one
  (#poke BIGNUM, top) bnptr one
  (#poke BIGNUM, dmax) bnptr one
  (#poke BIGNUM, neg) bnptr (if (I## v) < 0 then one else 0)
  return (wrapBN bnptr)

integerToBN v@(J## nlimbs_ bytearray)
  | v >= 0 = do
      let nlimbs = (I## nlimbs_)
      bnptr <- mallocBytes (#size BIGNUM)
      limbs <- mallocBytes ((#size unsigned long) * nlimbs)
      (#poke BIGNUM, d) bnptr limbs
      (#poke BIGNUM, flags) bnptr (1 :: CInt)
      _ <- _copy_out limbs bytearray (fromIntegral $ (#size unsigned long) * nlimbs)
      (#poke BIGNUM, top) bnptr ((fromIntegral nlimbs) :: CInt)
      (#poke BIGNUM, dmax) bnptr ((fromIntegral nlimbs) :: CInt)
      (#poke BIGNUM, neg) bnptr (0 :: CInt)
      return (wrapBN bnptr)
  | otherwise = do bnptr <- integerToBN (0-v)
                   (#poke BIGNUM, neg) (unwrapBN bnptr) (1 :: CInt)
                   return bnptr

#endif

-- TODO: we could make a function which doesn't even allocate BN data if we
-- wanted to be very fast and dangerout. The BIGNUM could point right into the
-- Integer's data. However, I'm not sure about the semantics of the GC; which
-- might move the Integer data around.

-- |@'withBN' n f@ converts n to a 'BigNum' and computes @f@. Then it
-- frees the 'BigNum'.
withBN :: Integer -> (BigNum -> IO a) -> IO a
withBN dec m = bracket (integerToBN dec) (_free . unwrapBN) m

foreign import ccall unsafe "BN_bn2mpi"
        _bn2mpi :: Ptr BIGNUM -> Ptr CChar -> IO CInt

foreign import ccall unsafe "BN_mpi2bn"
        _mpi2bn :: Ptr CChar -> CInt -> Ptr BIGNUM -> IO (Ptr BIGNUM)

-- |This is an alias to 'bnToInteger'.
peekBN :: BigNum -> IO Integer
peekBN = bnToInteger

-- |This is an alias to 'integerToBN'.
newBN :: Integer -> IO BigNum
newBN = integerToBN

-- | Convert a BigNum to an MPI: a serialisation of large ints which has a
--   4-byte, big endian length followed by the bytes of the number in
--   most-significant-first order.
bnToMPI :: BigNum -> IO BS.ByteString
bnToMPI bn = do
  bytes <- _bn2mpi (unwrapBN bn) nullPtr
  allocaBytes (fromIntegral bytes) (\buffer -> do
    _ <- _bn2mpi (unwrapBN bn) buffer
    BS.packCStringLen (buffer, fromIntegral bytes))

-- | Convert an MPI into a BigNum. See bnToMPI for details of the format
mpiToBN :: BS.ByteString -> IO BigNum
mpiToBN mpi = do
  BS.useAsCStringLen mpi (\(ptr, len) -> do
    _mpi2bn ptr (fromIntegral len) nullPtr) >>= return . wrapBN

-- | Convert an Integer to an MPI. See bnToMPI for the format
integerToMPI :: Integer -> IO BS.ByteString
integerToMPI v = bracket (integerToBN v) (_free . unwrapBN) bnToMPI

-- | Convert an MPI to an Integer. See bnToMPI for the format
mpiToInteger :: BS.ByteString -> IO Integer
mpiToInteger mpi = do
  bn <- mpiToBN mpi
  v <- bnToInteger bn
  _free (unwrapBN bn)
  return v

foreign import ccall unsafe "BN_mod_exp"
        _mod_exp :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> BNCtx -> IO (Ptr BIGNUM)

type BNCtx = Ptr BNCTX
data BNCTX

foreign import ccall unsafe "BN_CTX_new"
        _BN_ctx_new :: IO BNCtx

foreign import ccall unsafe "BN_CTX_free"
        _BN_ctx_free :: BNCtx -> IO ()

withBNCtx :: (BNCtx -> IO a) -> IO a
withBNCtx f = bracket _BN_ctx_new _BN_ctx_free f

-- |@'modexp' a p m@ computes @a@ to the @p@-th power modulo @m@.
modexp :: Integer -> Integer -> Integer -> Integer
modexp a p m = unsafePerformIO (do
  withBN a (\bnA -> (do
    withBN p (\bnP -> (do
      withBN m (\bnM -> (do
        withBNCtx (\ctx -> (do
          r <- newBN 0
          _ <- _mod_exp (unwrapBN r) (unwrapBN bnA) (unwrapBN bnP) (unwrapBN bnM) ctx
          bnToInteger r >>= return)))))))))

{- Random Integer generation ------------------------------------------------ -}

foreign import ccall unsafe "BN_rand_range"
        _BN_rand_range :: Ptr BIGNUM -> Ptr BIGNUM -> IO CInt

foreign import ccall unsafe "BN_pseudo_rand_range"
        _BN_pseudo_rand_range :: Ptr BIGNUM -> Ptr BIGNUM -> IO CInt

-- | Return a strongly random number in the range 0 <= x < n where the given
--   filter function returns true.
randIntegerUptoNMinusOneSuchThat :: (Integer -> Bool)  -- ^ a filter function
                                 -> Integer  -- ^ one plus the upper limit
                                 -> IO Integer
randIntegerUptoNMinusOneSuchThat f range = withBN range (\bnRange -> (do
  r <- newBN 0
  let try = do
        _BN_rand_range (unwrapBN r) (unwrapBN bnRange) >>= failIf_ (/= 1)
        i <- bnToInteger r
        if f i
           then return i
           else try
  try))

-- | Return a random number in the range 0 <= x < n where the given
--   filter function returns true.
prandIntegerUptoNMinusOneSuchThat :: (Integer -> Bool)  -- ^ a filter function
                                  -> Integer  -- ^ one plus the upper limit
                                  -> IO Integer
prandIntegerUptoNMinusOneSuchThat f range = withBN range (\bnRange -> (do
  r <- newBN 0
  let try = do
        _BN_rand_range (unwrapBN r) (unwrapBN bnRange) >>= failIf_ (/= 1)
        i <- bnToInteger r
        if f i
           then return i
           else try
  try))

-- | Return a strongly random number in the range 0 <= x < n
randIntegerZeroToNMinusOne :: Integer -> IO Integer
randIntegerZeroToNMinusOne = randIntegerUptoNMinusOneSuchThat (const True)
-- | Return a strongly random number in the range 0 < x < n
randIntegerOneToNMinusOne :: Integer -> IO Integer
randIntegerOneToNMinusOne = randIntegerUptoNMinusOneSuchThat (/= 0)

-- | Return a random number in the range 0 <= x < n
prandIntegerZeroToNMinusOne :: Integer -> IO Integer
prandIntegerZeroToNMinusOne = prandIntegerUptoNMinusOneSuchThat (const True)
-- | Return a random number in the range 0 < x < n
prandIntegerOneToNMinusOne :: Integer -> IO Integer
prandIntegerOneToNMinusOne = prandIntegerUptoNMinusOneSuchThat (/= 0)
