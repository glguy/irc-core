-- | Tests for the non-EVP ciphers
module Main (main) where
import qualified Data.ByteString as BS
import OpenSSL.Cipher
import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TF
import Test.HUnit

-- | Convert a hex string to a ByteString (e.g. "0011" == BS.pack [0, 0x11])
hexToBS :: String -> BS.ByteString
hexToBS [] = BS.empty
hexToBS (a : b : rest) = BS.append (BS.singleton ((valueOfHexChar a * 16) + valueOfHexChar b))
                                         (hexToBS rest)
hexToBS xs = error ("hexToBS: invalid hex string: " ++ xs)

valueOfHexChar :: Integral a => Char -> a
valueOfHexChar '0' = 0
valueOfHexChar '1' = 1
valueOfHexChar '2' = 2
valueOfHexChar '3' = 3
valueOfHexChar '4' = 4
valueOfHexChar '5' = 5
valueOfHexChar '6' = 6
valueOfHexChar '7' = 7
valueOfHexChar '8' = 8
valueOfHexChar '9' = 9
valueOfHexChar 'a' = 10
valueOfHexChar 'b' = 11
valueOfHexChar 'c' = 12
valueOfHexChar 'd' = 13
valueOfHexChar 'e' = 14
valueOfHexChar 'f' = 15
valueOfHexChar 'A' = 10
valueOfHexChar 'B' = 11
valueOfHexChar 'C' = 12
valueOfHexChar 'D' = 13
valueOfHexChar 'E' = 14
valueOfHexChar 'F' = 15
valueOfHexChar x   = error ("valueOfHexChar: invalid char: " ++ show x)

-- | A test containing counter mode test vectors
data CTRTest = CTRTest BS.ByteString  -- ^ key
                       BS.ByteString  -- ^ IV
                       BS.ByteString  -- ^ plaintext
                       BS.ByteString  -- ^ cipher text

-- Test vectors from draft-ietf-ipsec-ciph-aes-ctr-05 section 6
ctrTests :: [CTRTest]
ctrTests = [
  CTRTest (hexToBS "AE6852F8121067CC4BF7A5765577F39E")
          (hexToBS "00000030000000000000000000000001")
          (hexToBS "53696E676C6520626C6F636B206D7367")
          (hexToBS "E4095D4FB7A7B3792D6175A3261311B8"),
  CTRTest (hexToBS "7691BE035E5020A8AC6E618529F9A0DC")
          (hexToBS "00E0017B27777F3F4A1786F000000001")
          (hexToBS "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F20212223")
          (hexToBS "C1CF48A89F2FFDD9CF4652E9EFDB72D74540A42BDE6D7836D59A5CEAAEF3105325B2072F"),
  CTRTest (hexToBS "16AF5B145FC9F579C175F93E3BFB0EED863D06CCFDB78515")
          (hexToBS "0000004836733C147D6D93CB00000001")
          (hexToBS "53696E676C6520626C6F636B206D7367")
          (hexToBS "4B55384FE259C9C84E7935A003CBE928"),
  CTRTest (hexToBS "FF7A617CE69148E4F1726E2F43581DE2AA62D9F805532EDFF1EED687FB54153D")
          (hexToBS "001CC5B751A51D70A1C1114800000001")
          (hexToBS "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F20212223")
          (hexToBS "EB6C52821D0BBBF7CE7594462ACA4FAAB407DF866569FD07F48CC0B583D6071F1EC0E6B8") ]

runCtrTest :: CTRTest -> Test
runCtrTest (CTRTest key iv plaintext ciphertext) =
    TestCase $ do
      ctx <- newAESCtx Encrypt key iv
      ct  <- aesCTR ctx plaintext
      assertEqual "" ciphertext ct

tests :: Test
tests = TestList $ map runCtrTest ctrTests

main :: IO ()
main = TF.defaultMain $ TF.hUnitTestToTests tests
