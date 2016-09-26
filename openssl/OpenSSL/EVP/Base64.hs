{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |An interface to Base64 codec.
module OpenSSL.EVP.Base64
    ( -- * Encoding
      encodeBase64
    , encodeBase64BS
    , encodeBase64LBS

      -- * Decoding
    , decodeBase64
    , decodeBase64BS
    , decodeBase64LBS
    )
    where
import           Control.Exception (assert)
import           Data.ByteString.Internal (createAndTrim)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import qualified Data.ByteString.Lazy.Internal as L8Internal
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List
#if MIN_VERSION_base(4,5,0)
import           Foreign.C.Types (CChar(..), CInt(..))
#else
import           Foreign.C.Types (CChar, CInt)
#endif
import           Foreign.Ptr (Ptr, castPtr)
import           System.IO.Unsafe (unsafePerformIO)


-- On encoding, we keep fetching the next block until we get at least
-- 3 bytes. Then we apply B8.concat to the returned [ByteString] and
-- split it at the offset in multiple of 3, then prepend the remaining
-- bytes to the next block.
--
-- On decoding, we apply the same algorithm but we split the input in
-- multiple of 4.
nextBlock :: Int -> ([B8.ByteString], L8.ByteString) -> ([B8.ByteString], L8.ByteString)
nextBlock minLen (xs, src)
    = if foldl' (+) 0 (map B8.length xs) >= minLen then
          (xs, src)
      else
          case src of
            L8Internal.Empty      -> (xs, src)
            L8Internal.Chunk y ys -> nextBlock minLen (xs ++ [y], ys)


{- encode -------------------------------------------------------------------- -}

foreign import ccall unsafe "EVP_EncodeBlock"
        _EncodeBlock :: Ptr CChar -> Ptr CChar -> CInt -> IO CInt


encodeBlock :: B8.ByteString -> B8.ByteString
encodeBlock inBS
    = unsafePerformIO $
      unsafeUseAsCStringLen inBS $ \ (inBuf, inLen) ->
      createAndTrim maxOutLen $ \ outBuf ->
      fmap fromIntegral
           (_EncodeBlock (castPtr outBuf) inBuf (fromIntegral inLen))
    where
      maxOutLen = (inputLen `div` 3 + 1) * 4 + 1 -- +1: '\0'
      inputLen  = B8.length inBS


-- |@'encodeBase64' str@ lazilly encodes a stream of data to
-- Base64. The string doesn't have to be finite. Note that the string
-- must not contain any letters which aren't in the range of U+0000 -
-- U+00FF.
{-# DEPRECATED encodeBase64 "Use encodeBase64BS or encodeBase64LBS instead." #-}
encodeBase64 :: String -> String
encodeBase64 = L8.unpack . encodeBase64LBS . L8.pack

-- |@'encodeBase64BS' bs@ strictly encodes a chunk of data to Base64.
encodeBase64BS :: B8.ByteString -> B8.ByteString
encodeBase64BS = encodeBlock

-- |@'encodeBase64LBS' lbs@ lazilly encodes a stream of data to
-- Base64. The string doesn't have to be finite.
encodeBase64LBS :: L8.ByteString -> L8.ByteString
encodeBase64LBS inLBS
    | L8.null inLBS = L8.empty
    | otherwise
        = let (blockParts', remain' ) = nextBlock 3 ([], inLBS)
              block'                  = B8.concat blockParts'
              blockLen'               = B8.length block'
              (block      , leftover) = if blockLen' < 3 then
                                            -- The last remnant.
                                            (block', B8.empty)
                                        else
                                            B8.splitAt (blockLen' - blockLen' `mod` 3) block'
              remain                  = if B8.null leftover then
                                            remain'
                                        else
					    L8.fromChunks [leftover] `L8.append` remain'
              encodedBlock             = encodeBlock block
              encodedRemain            = encodeBase64LBS remain
          in
            L8.fromChunks [encodedBlock] `L8.append` encodedRemain


{- decode -------------------------------------------------------------------- -}

foreign import ccall unsafe "EVP_DecodeBlock"
        _DecodeBlock :: Ptr CChar -> Ptr CChar -> CInt -> IO CInt


decodeBlock :: B8.ByteString -> B8.ByteString
decodeBlock inBS
    = assert (B8.length inBS `mod` 4 == 0) $
      unsafePerformIO $
      unsafeUseAsCStringLen inBS $ \ (inBuf, inLen) ->
      createAndTrim (B8.length inBS) $ \ outBuf ->
      _DecodeBlock (castPtr outBuf) inBuf (fromIntegral inLen)
           >>= \ outLen -> return (fromIntegral outLen - paddingLen)
    where
      paddingLen :: Int
      paddingLen = B8.count '=' inBS

-- |@'decodeBase64' str@ lazilly decodes a stream of data from
-- Base64. The string doesn't have to be finite.
{-# DEPRECATED decodeBase64 "Use decodeBase64BS or decodeBase64LBS instead." #-}
decodeBase64 :: String -> String
decodeBase64 = L8.unpack . decodeBase64LBS . L8.pack

-- |@'decodeBase64BS' bs@ strictly decodes a chunk of data from
-- Base64.
decodeBase64BS :: B8.ByteString -> B8.ByteString
decodeBase64BS = decodeBlock

-- |@'decodeBase64LBS' lbs@ lazilly decodes a stream of data from
-- Base64. The string doesn't have to be finite.
decodeBase64LBS :: L8.ByteString -> L8.ByteString
decodeBase64LBS inLBS
    | L8.null inLBS = L8.empty
    | otherwise
        = let (blockParts', remain' ) = nextBlock 4 ([], inLBS)
              block'                  = B8.concat blockParts'
              blockLen'               = B8.length block'
              (block      , leftover) = assert (blockLen' >= 4) $
                                        B8.splitAt (blockLen' - blockLen' `mod` 4) block'
              remain                  = if B8.null leftover then
                                            remain'
                                        else
					    L8.fromChunks [leftover] `L8.append` remain'
              decodedBlock            = decodeBlock block
              decodedRemain           = decodeBase64LBS remain
          in
            L8.fromChunks [decodedBlock] `L8.append` decodedRemain
