module Main (main) where
import qualified Data.ByteString as BS
import OpenSSL.DSA
import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TF
import Test.HUnit

-- | This function just runs the example DSA generation, as given in FIP 186-2,
--   app 5.
test_generateParameters :: Test
test_generateParameters = TestCase $ do
  let seed = BS.pack [0xd5, 0x01, 0x4e, 0x4b,
                      0x60, 0xef, 0x2b, 0xa8,
                      0xb6, 0x21, 0x1b, 0x40,
                      0x62, 0xba, 0x32, 0x24,
                      0xe0, 0x42, 0x7d, 0xd3]
  (a, _, p, q, g) <- generateDSAParameters 512 $ Just seed
  assertEqual "generateParameters"
              ( 105
              , 0x8df2a494492276aa3d25759bb06869cbeac0d83afb8d0cf7cbb8324f0d7882e5d0762fc5b7210eafc2e9adac32ab7aac49693dfbf83724c2ec0736ee31c80291
              , 0xc773218c737ec8ee993b4f2ded30f48edace915f
              , 0x626d027839ea0a13413163a55b4cb500299d5522956cefcb3bff10f399ce2c2e71cb9de5fa24babf58e5b79521925c9cc42e9f6f464b088cc572af53e6d78802
              ) (a, p, q, g)

testMessage :: BS.ByteString
testMessage = BS.pack [1..20]

test_signVerify :: Test
test_signVerify = TestCase $ do
  dsa    <- generateDSAParametersAndKey 512 Nothing
  (a, b) <- signDigestedDataWithDSA dsa testMessage
  valid  <- verifyDigestedDataWithDSA dsa testMessage (a, b)
  assertBool "signVerify" valid

tests :: Test
tests = TestList [test_generateParameters, test_signVerify]

main :: IO ()
main = TF.defaultMain $ TF.hUnitTestToTests tests
