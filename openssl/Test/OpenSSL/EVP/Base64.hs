{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Unittest for Base64 [en|de]coding.
module Main (main) where
#if !MIN_VERSION_bytestring(0,9,1)
import Data.Char (ord)
import Data.String
#endif
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import OpenSSL.EVP.Base64
import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TF
import Test.HUnit

-- NOTE: bytestring-0.9.0.4 has these instances too, while
-- bytestring-0.9.0.3 does not. If our bytestring is 0.9.0.4 we'll
-- have duplicate instances, but that's not our fault, is it?
#if !MIN_VERSION_bytestring(0,9,1)
instance IsString BS.ByteString where
  fromString = BS.pack . map (fromIntegral . ord)

-- Note that this instance packs each charactor as a separate lazy chunk.
-- This is to stress the lazy code - not because it's a good idea generally
instance IsString BSL.ByteString where
  fromString = BSL.fromChunks . map (BS.singleton . fromIntegral . ord)
#endif

encodeTests :: Test
encodeTests =
    TestLabel "encode" $
    TestList $ map (\(a, v) -> encodeBase64BS a ~?= v) pairs
    where
      pairs :: [(BS.ByteString, BS.ByteString)]
      pairs = [ (""   , ""    )
              , ("a"  , "YQ==")
              , ("aa" , "YWE=")
              , ("aaa", "YWFh")
              ]

lazyEncodeTests :: Test
lazyEncodeTests =
    TestLabel "lazyEncode" $
    TestList $ map (\(a, v) -> encodeBase64LBS a ~?= v) pairs
    where
      pairs :: [(BSL.ByteString, BSL.ByteString)]
      pairs = [ (""   , ""    )
              , ("a"  , "YQ==")
              , ("aa" , "YWE=")
              , ("aaa", "YWFh")
              ]

decodeTests :: Test
decodeTests =
    TestLabel "decode" $
    TestList $ map (\(a, v) -> decodeBase64BS a ~?= v) pairs
    where
      pairs :: [(BS.ByteString, BS.ByteString)]
      pairs = [ (""                  , ""           )
              , ("aGFza2VsbA=="      , "haskell"    )
              , ("YWJjZGVmZ2hpams="  , "abcdefghijk")
              , ("YWJjZGVmZ2hpams=\n", "abcdefghijk")
              ]

tests :: Test
tests = TestList
        [ encodeTests
        , lazyEncodeTests
        , decodeTests
        ]

main :: IO ()
main = TF.defaultMain $ TF.hUnitTestToTests tests
