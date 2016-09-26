{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{- --------------------------------------------------------------------------- -}
{-                                                                             -}
{-                           FOR INTERNAL USE ONLY                             -}
{-                                                                             -}
{- When I firstly saw the manpage of bio(3), it looked like a great API. I ac- -}
{- tually wrote a wrapper and even wrote a document. What a pain!              -}
{-                                                                             -}
{- Now I realized that BIOs aren't necessary to we Haskell hackers. Their fun- -}
{- ctionalities overlaps with Haskell's own I/O system. The only thing which   -}
{- wasn't available without bio(3) -- at least I thought so -- was the         -}
{- BIO_f_base64(3), but I found an undocumented API for the Base64 codec.      -}
{-          I FOUND AN UNDOCUMENTED API FOR THE VERY BASE64 CODEC.             -}
{- So I decided to bury all the OpenSSL.BIO module. The game is over.          -}
{-                                                                             -}
{- --------------------------------------------------------------------------- -}


-- |A BIO is an I\/O abstraction, it hides many of the underlying I\/O
-- details from an application, if you are writing a pure C
-- application...
--
-- I know, we are hacking on Haskell so BIO components like BIO_s_file
-- are hardly needed. But for filter BIOs, such as BIO_f_base64 and
-- BIO_f_cipher, they should be useful too to us.

module OpenSSL.BIO
    ( -- * Type
      BIO
    , BIO_

    , wrapBioPtr  -- private
    , withBioPtr  -- private
    , withBioPtr' -- private

      -- * BIO chaning
    , bioPush
    , (==>)
    , (<==)
    , bioJoin

      -- * BIO control operations
    , bioFlush
    , bioReset
    , bioEOF

      -- * BIO I\/O functions
    , bioRead
    , bioReadBS
    , bioReadLBS
    , bioGets
    , bioGetsBS
    , bioGetsLBS
    , bioWrite
    , bioWriteBS
    , bioWriteLBS

      -- * Base64 BIO filter
    , newBase64

      -- * Buffering BIO filter
    , newBuffer

      -- * Memory BIO sink\/source
    , newMem
    , newConstMem
    , newConstMemBS
    , newConstMemLBS

      -- * Null data BIO sink\/source
    , newNullBIO
    )
    where

import           Control.Monad
import           Data.ByteString.Internal (createAndTrim, toForeignPtr)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.ByteString.Lazy.Internal as L
import           Foreign                       hiding (new)
import           Foreign.C
import           Foreign.Concurrent            as Conc
import           OpenSSL.Utils
import           System.IO.Unsafe

{- bio ---------------------------------------------------------------------- -}

data    BIO_METHOD

-- |@BIO@ is a @ForeignPtr@ to an opaque BIO object. They are created by newXXX actions.
newtype BIO  = BIO (ForeignPtr BIO_)
data    BIO_

foreign import ccall unsafe "BIO_new"
        _new :: Ptr BIO_METHOD -> IO (Ptr BIO_)

foreign import ccall unsafe "BIO_free"
        _free :: Ptr BIO_ -> IO ()

foreign import ccall unsafe "BIO_push"
        _push :: Ptr BIO_ -> Ptr BIO_ -> IO (Ptr BIO_)

foreign import ccall unsafe "HsOpenSSL_BIO_set_flags"
        _set_flags :: Ptr BIO_ -> CInt -> IO ()

foreign import ccall unsafe "HsOpenSSL_BIO_should_retry"
        _should_retry :: Ptr BIO_ -> IO CInt


new :: Ptr BIO_METHOD -> IO BIO
new method
    = _new method >>= failIfNull >>= wrapBioPtr


wrapBioPtr :: Ptr BIO_ -> IO BIO
wrapBioPtr bioPtr
    = fmap BIO (Conc.newForeignPtr bioPtr (_free bioPtr))


withBioPtr :: BIO -> (Ptr BIO_ -> IO a) -> IO a
withBioPtr (BIO bio) = withForeignPtr bio


withBioPtr' :: Maybe BIO -> (Ptr BIO_ -> IO a) -> IO a
withBioPtr' Nothing    f = f nullPtr
withBioPtr' (Just bio) f = withBioPtr bio f


-- Connect 'b' behind 'a'. It's possible that 1. we only retain 'a'
-- and write to 'a', and 2. we only retain 'b' and read from 'b', so
-- both ForeignPtr's have to touch each other. This involves a
-- circular dependency but that won't be a problem as the garbage
-- collector isn't reference-counting.

-- |Computation of @'bioPush' a b@ connects @b@ behind @a@.
--
-- Example:
--
-- > do b64 <- newBase64 True
-- >    mem <- newMem
-- >    bioPush b64 mem
-- >
-- >    -- Encode some text in Base64 and write the result to the
-- >    -- memory buffer.
-- >    bioWrite b64 "Hello, world!"
-- >    bioFlush b64
-- >
-- >    -- Then dump the memory buffer.
-- >    bioRead mem >>= putStrLn
--
bioPush :: BIO -> BIO -> IO ()
bioPush (BIO a) (BIO b)
    = withForeignPtr a $ \ aPtr ->
      withForeignPtr b $ \ bPtr ->
      do _ <- _push aPtr bPtr
         Conc.addForeignPtrFinalizer a $ touchForeignPtr b
         Conc.addForeignPtrFinalizer b $ touchForeignPtr a
         return ()

-- |@a '==>' b@ is an alias to @'bioPush' a b@.
(==>) :: BIO -> BIO -> IO ()
(==>) = bioPush

-- |@a '<==' b@ is an alias to @'bioPush' b a@.
(<==) :: BIO -> BIO -> IO ()
(<==) = flip bioPush


-- |@'bioJoin' [bio1, bio2, ..]@ connects many BIOs at once.
bioJoin :: [BIO] -> IO ()
bioJoin []       = return ()
bioJoin (_:[])   = return ()
bioJoin (a:b:xs) = bioPush a b >> bioJoin (b:xs)


setFlags :: BIO -> CInt -> IO ()
setFlags bio flags
    = withBioPtr bio $ flip _set_flags flags
      

bioShouldRetry :: BIO -> IO Bool
bioShouldRetry bio
    = withBioPtr bio $ \ bioPtr ->
      fmap (/= 0) (_should_retry bioPtr)


{- ctrl --------------------------------------------------------------------- -}

foreign import ccall unsafe "HsOpenSSL_BIO_flush"
        _flush :: Ptr BIO_ -> IO CInt

foreign import ccall unsafe "HsOpenSSL_BIO_reset"
        _reset :: Ptr BIO_ -> IO CInt

foreign import ccall unsafe "HsOpenSSL_BIO_eof"
        _eof :: Ptr BIO_ -> IO CInt

-- |@'bioFlush' bio@ normally writes out any internally buffered data,
-- in some cases it is used to signal EOF and that no more data will
-- be written.
bioFlush :: BIO -> IO ()
bioFlush bio
    = withBioPtr bio $ \ bioPtr ->
      _flush bioPtr >>= failIf (/= 1) >> return ()

-- |@'bioReset' bio@ typically resets a BIO to some initial state.
bioReset :: BIO -> IO ()
bioReset bio
    = withBioPtr bio $ \ bioPtr ->
      _reset bioPtr >> return () -- Return value of BIO_reset is not
                                 -- consistent in every BIO's so we
                                 -- can't do error-checking.

-- |@'bioEOF' bio@ returns 1 if @bio@ has read EOF, the precise
-- meaning of EOF varies according to the BIO type.
bioEOF :: BIO -> IO Bool
bioEOF bio
    = withBioPtr bio $ \ bioPtr ->
      fmap (==1) (_eof bioPtr)


{- I/O ---------------------------------------------------------------------- -}

foreign import ccall unsafe "BIO_read"
        _read :: Ptr BIO_ -> Ptr CChar -> CInt -> IO CInt

foreign import ccall unsafe "BIO_gets"
        _gets :: Ptr BIO_ -> Ptr CChar -> CInt -> IO CInt

foreign import ccall unsafe "BIO_write"
        _write :: Ptr BIO_ -> Ptr CChar -> CInt -> IO CInt

-- |@'bioRead' bio@ lazily reads all data in @bio@.
bioRead :: BIO -> IO String
bioRead bio
    = liftM L.unpack $ bioReadLBS bio

-- |@'bioReadBS' bio len@ attempts to read @len@ bytes from @bio@,
-- then return a ByteString. The actual length of result may be less
-- than @len@.
bioReadBS :: BIO -> Int -> IO B.ByteString
bioReadBS bio maxLen
    = withBioPtr bio       $ \ bioPtr ->
      createAndTrim maxLen $ \ bufPtr ->
      _read bioPtr (castPtr bufPtr) (fromIntegral maxLen) >>= interpret
    where
      interpret :: CInt -> IO Int
      interpret n
          | n ==  0   = return 0
          | n == -1   = return 0
          | n <  -1   = raiseOpenSSLError
          | otherwise = return (fromIntegral n)

-- |@'bioReadLBS' bio@ lazily reads all data in @bio@, then return a
-- LazyByteString.
bioReadLBS :: BIO -> IO L.ByteString
bioReadLBS bio = fmap L.fromChunks lazyRead
    where
      chunkSize = L.defaultChunkSize
      
      lazyRead = unsafeInterleaveIO loop

      loop = do bs <- bioReadBS bio chunkSize
                if B.null bs then
                    do isEOF <- bioEOF bio
                       if isEOF then
                           return []
                         else
                           do shouldRetry <- bioShouldRetry bio
                              if shouldRetry then
                                  loop
                                else
                                  fail "bioReadLBS: got null but isEOF=False, shouldRetry=False"
                  else
                    do bss <- lazyRead
                       return (bs:bss)

-- |@'bioGets' bio len@ normally attempts to read one line of data
-- from @bio@ of maximum length @len@. There are exceptions to this
-- however, for example 'bioGets' on a digest BIO will calculate and
-- return the digest and other BIOs may not support 'bioGets' at all.
bioGets :: BIO -> Int -> IO String
bioGets bio maxLen
    = liftM B.unpack (bioGetsBS bio maxLen)

-- |'bioGetsBS' does the same as 'bioGets' but returns ByteString.
bioGetsBS :: BIO -> Int -> IO B.ByteString
bioGetsBS bio maxLen
    = withBioPtr bio       $ \ bioPtr ->
      createAndTrim maxLen $ \ bufPtr ->
      _gets bioPtr (castPtr bufPtr) (fromIntegral maxLen) >>= interpret
    where
      interpret :: CInt -> IO Int
      interpret n
          | n ==  0   = return 0
          | n == -1   = return 0
          | n <  -1   = raiseOpenSSLError
          | otherwise = return (fromIntegral n)

-- |'bioGetsLBS' does the same as 'bioGets' but returns
-- LazyByteString.
bioGetsLBS :: BIO -> Int -> IO L.ByteString
bioGetsLBS bio maxLen
    = bioGetsBS bio maxLen >>= \ bs -> (return . L.fromChunks) [bs]

-- |@'bioWrite' bio str@ lazily writes entire @str@ to @bio@. The
-- string doesn't necessarily have to be finite.
bioWrite :: BIO -> String -> IO ()
bioWrite bio str
    = (return . L.pack) str >>= bioWriteLBS bio

-- |@'bioWriteBS' bio bs@ writes @bs@ to @bio@.
bioWriteBS :: BIO -> B.ByteString -> IO ()
bioWriteBS bio bs
    = withBioPtr bio           $ \ bioPtr ->
      unsafeUseAsCStringLen bs $ \ (buf, len) ->
      _write bioPtr buf (fromIntegral len) >>= interpret
    where
      interpret :: CInt -> IO ()
      interpret n
          | n == fromIntegral (B.length bs)
                      = return ()
          | n == -1   = bioWriteBS bio bs -- full retry
          | n <  -1   = raiseOpenSSLError
          | otherwise = bioWriteBS bio (B.drop (fromIntegral n) bs) -- partial retry

-- |@'bioWriteLBS' bio lbs@ lazily writes entire @lbs@ to @bio@. The
-- string doesn't necessarily have to be finite.
bioWriteLBS :: BIO -> L.ByteString -> IO ()
bioWriteLBS bio lbs
    = mapM_ (bioWriteBS bio) $ L.toChunks lbs


{- base64 ------------------------------------------------------------------- -}

foreign import ccall unsafe "BIO_f_base64"
        f_base64 :: IO (Ptr BIO_METHOD)

foreign import ccall unsafe "HsOpenSSL_BIO_FLAGS_BASE64_NO_NL"
        _FLAGS_BASE64_NO_NL :: CInt

-- |@'newBase64' noNL@ creates a Base64 BIO filter. This is a filter
-- bio that base64 encodes any data written through it and decodes any
-- data read through it.
--
-- If @noNL@ flag is True, the filter encodes the data all on one line
-- or expects the data to be all on one line.
--
-- Base64 BIOs do not support 'bioGets'.
--
-- 'bioFlush' on a Base64 BIO that is being written through is used to
-- signal that no more data is to be encoded: this is used to flush
-- the final block through the BIO.
newBase64 :: Bool -> IO BIO
newBase64 noNL
    = do bio <- new =<< f_base64
         when noNL $ setFlags bio _FLAGS_BASE64_NO_NL
         return bio


{- buffer ------------------------------------------------------------------- -}

foreign import ccall unsafe "BIO_f_buffer"
        f_buffer :: IO (Ptr BIO_METHOD)

foreign import ccall unsafe "HsOpenSSL_BIO_set_buffer_size"
        _set_buffer_size :: Ptr BIO_ -> CInt -> IO CInt


-- |@'newBuffer' mBufSize@ creates a buffering BIO filter. Data
-- written to a buffering BIO is buffered and periodically written to
-- the next BIO in the chain. Data read from a buffering BIO comes
-- from the next BIO in the chain.
--
-- Buffering BIOs support 'bioGets'.
--
-- Calling 'bioReset' on a buffering BIO clears any buffered data.
--
-- Question: When I created a BIO chain like this and attempted to
-- read from the buf, the buffering BIO weirdly behaved: BIO_read()
-- returned nothing, but both BIO_eof() and BIO_should_retry()
-- returned zero. I tried to examine the source code of
-- crypto\/bio\/bf_buff.c but it was too complicated to
-- understand. Does anyone know why this happens? The version of
-- OpenSSL was 0.9.7l.
--
-- > main = withOpenSSL $
-- >        do mem <- newConstMem "Hello, world!"
-- >           buf <- newBuffer Nothing
-- >           mem ==> buf
-- >
-- >           bioRead buf >>= putStrLn -- This fails, but why?
--
-- I am being depressed for this unaccountable failure.
--
newBuffer :: Maybe Int -- ^ Explicit buffer size (@Just n@) or the
                       -- default size (@Nothing@).
          -> IO BIO
newBuffer bufSize
    = do bio <- new =<< f_buffer
         case bufSize of
           Just n  -> withBioPtr bio $ \ bioPtr ->
                      _set_buffer_size bioPtr (fromIntegral n)
                           >>= failIf (/= 1) >> return ()
           Nothing -> return ()
         return bio


{- mem ---------------------------------------------------------------------- -}

foreign import ccall unsafe "BIO_s_mem"
        s_mem :: IO (Ptr BIO_METHOD)

foreign import ccall unsafe "BIO_new_mem_buf"
        _new_mem_buf :: Ptr CChar -> CInt -> IO (Ptr BIO_)


-- |@'newMem'@ creates a memory BIO sink\/source. Any data written to
-- a memory BIO can be recalled by reading from it. Unless the memory
-- BIO is read only any data read from it is deleted from the BIO.
--
-- Memory BIOs support 'bioGets'.
--
-- Calling 'bioReset' on a read write memory BIO clears any data in
-- it. On a read only BIO it restores the BIO to its original state
-- and the read only data can be read again.
--
-- 'bioEOF' is true if no data is in the BIO.
--
-- Every read from a read write memory BIO will remove the data just
-- read with an internal copy operation, if a BIO contains a lots of
-- data and it is read in small chunks the operation can be very
-- slow. The use of a read only memory BIO avoids this problem. If the
-- BIO must be read write then adding a buffering BIO ('newBuffer') to
-- the chain will speed up the process.
newMem :: IO BIO
newMem = s_mem >>= new

-- |@'newConstMem' str@ creates a read-only memory BIO source.
newConstMem :: String -> IO BIO
newConstMem str = newConstMemBS (B.pack str)

-- |@'newConstMemBS' bs@ is like 'newConstMem' but takes a ByteString.
newConstMemBS :: B.ByteString -> IO BIO
newConstMemBS bs
    = let (foreignBuf, off, len) = toForeignPtr bs
      in
        -- Let the BIO's finalizer have a reference to the ByteString.
        withForeignPtr foreignBuf $ \ buf ->
        do bioPtr <- _new_mem_buf (castPtr $ buf `plusPtr` off) (fromIntegral len)
                     >>= failIfNull

           bio <- newForeignPtr_ bioPtr
           Conc.addForeignPtrFinalizer bio (_free bioPtr >> touchForeignPtr foreignBuf)
           
           return $ BIO bio

-- |@'newConstMemLBS' lbs@ is like 'newConstMem' but takes a
-- LazyByteString.
newConstMemLBS :: L.ByteString -> IO BIO
newConstMemLBS lbs
    = (return . B.concat . L.toChunks) lbs >>= newConstMemBS

{- null --------------------------------------------------------------------- -}

foreign import ccall unsafe "BIO_s_null"
        s_null :: IO (Ptr BIO_METHOD)

-- |@'newNullBIO'@ creates a null BIO sink\/source. Data written to
-- the null sink is discarded, reads return EOF.
--
-- A null sink is useful if, for example, an application wishes to
-- digest some data by writing through a digest bio but not send the
-- digested data anywhere. Since a BIO chain must normally include a
-- source\/sink BIO this can be achieved by adding a null sink BIO to
-- the end of the chain.
newNullBIO :: IO BIO
newNullBIO = s_null >>= new
