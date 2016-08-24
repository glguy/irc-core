{-|
Module      : Irc.Identifier
Description : Type and operations for nicknames and channel names
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module defines support for working with IRC's numeric reply
codes. Pattern synonyms are provided for each of the possible IRC reply codes.

Reply code information was extracted from https://www.alien.net.au/irc/irc2numerics.html

-}
module Irc.Identifier
  ( Identifier
  , idDenote
  , mkId
  , idText
  , idPrefix
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Char
import           Data.Function
import           Data.Hashable
import           Data.Primitive.ByteArray
import           Data.String
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Primitive as PV
import           Data.Word

-- | Identifier representing channels and nicknames
data Identifier = Identifier {-# UNPACK #-} !Text
                             {-# UNPACK #-} !(PV.Vector Word8)
  deriving (Read, Show)

-- | Equality on normalized identifier
instance Eq Identifier where
  (==) = (==) `on` idDenote

-- | Comparison on normalized identifier
instance Ord Identifier where
  compare = compare `on` idDenote

-- | Hash on normalized identifier
instance Hashable Identifier where
  hashWithSalt s = hashPV8WithSalt s . idDenote

instance IsString Identifier where
  fromString = mkId . fromString

hashPV8WithSalt :: Int -> PV.Vector Word8 -> Int
hashPV8WithSalt salt (PV.Vector off len (ByteArray arr)) =
  hashByteArrayWithSalt arr off len salt

-- | Construct an 'Identifier' from a 'ByteString'
mkId :: Text -> Identifier
mkId x = Identifier x (ircFoldCase (Text.encodeUtf8 x))

-- | Returns the original 'Text' of an 'Identifier'
idText :: Identifier -> Text
idText (Identifier x _) = x

-- | Returns the case-normalized 'ByteString' of an 'Identifier'
-- which is suitable for comparison or hashing.
idDenote :: Identifier -> PV.Vector Word8
idDenote (Identifier _ x) = x

-- | Returns 'True' when the first argument is a prefix of the second.
idPrefix :: Identifier -> Identifier -> Bool
idPrefix (Identifier _ x) (Identifier _ y) = x == PV.take (PV.length x) y

-- | Capitalize a string according to RFC 2812
-- Latin letters are capitalized and {|}~ are mapped to [\]^
ircFoldCase :: ByteString -> PV.Vector Word8
ircFoldCase = PV.fromList . map (\i -> casemap PV.! fromIntegral i) . B.unpack

casemap :: PV.Vector Word8
casemap
  = PV.fromList
  $ map (fromIntegral . ord)
  $ ['\x00'..'`'] ++ "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^" ++ ['\x7f'..'\xff']
