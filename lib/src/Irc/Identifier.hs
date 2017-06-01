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
  , idTextNorm
  , idPrefix
  ) where

import           Control.Monad.ST
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Char
import           Data.Foldable
import           Data.Function
import           Data.Hashable
import           Data.Monoid
import           Data.Primitive.ByteArray
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Primitive as PV
import qualified Data.Primitive.ByteArray as BA
import           Data.Primitive.ByteArray (ByteArray)
import           Data.Word

-- | Identifier representing channels and nicknames
data Identifier = Identifier {-# UNPACK #-} !Text
                             {-# UNPACK #-} !ByteArray

-- | This indexing function exists to specialize the type
-- of 'BA.indexByteArray'.
indexWord8 :: ByteArray -> Int -> Word8
indexWord8 = BA.indexByteArray

-- | Equality on normalized identifier
instance Eq Identifier where
  Identifier _ x == Identifier _ y =
    BA.sizeofByteArray x == BA.sizeofByteArray y &&
    all (\i -> indexWord8 x i == indexWord8 y i)
        [0 .. BA.sizeofByteArray x - 1]

-- | Show as string literal
instance Show Identifier where
  show = show . idText

-- | Read as string literal
instance Read Identifier where
  readsPrec p x = [ (mkId t, rest) | (t,rest) <- readsPrec p x]

-- | Comparison on normalized identifier
instance Ord Identifier where
  compare (Identifier _ x) (Identifier _ y) =
    mconcat [ indexWord8 x i `compare` indexWord8 y i | i <- [0..n-1]]
      <> (BA.sizeofByteArray x `compare` BA.sizeofByteArray y)
    where
      n = min (BA.sizeofByteArray x) (BA.sizeofByteArray y)

-- | Hash on normalized identifier
instance Hashable Identifier where
  hashWithSalt salt (Identifier _ b@(ByteArray arr)) =
    hashByteArrayWithSalt arr 0 (BA.sizeofByteArray b) salt

-- | @'fromString' = 'mkId' . 'fromString'
instance IsString Identifier where
  fromString = mkId . fromString

-- | Construct an 'Identifier' from a 'ByteString'
mkId :: Text -> Identifier
mkId x = Identifier x (ircFoldCase (Text.encodeUtf8 x))

-- | Returns the original 'Text' of an 'Identifier'
idText :: Identifier -> Text
idText (Identifier x _) = x

-- | Returns a 'ByteArray' of an 'Identifier'
-- which is suitable for comparison or hashing
-- which has been normalized for case.
idDenote :: Identifier -> ByteArray
idDenote (Identifier _ x) = x

-- | Returns the case-normalized 'Text' for an identifier.
idTextNorm :: Identifier -> Text
idTextNorm (Identifier _ x) =
  Text.decodeUtf8
    (B.pack [ indexWord8 x i | i <- [0 .. BA.sizeofByteArray x - 1]])

-- | Returns 'True' when the first argument is a prefix of the second.
idPrefix :: Identifier -> Identifier -> Bool
idPrefix (Identifier _ x) (Identifier _ y) =
  BA.sizeofByteArray x <= BA.sizeofByteArray y &&
  all (\i -> indexWord8 x i == indexWord8 y i)
      [0 .. BA.sizeofByteArray x - 1]

-- | Capitalize a string according to RFC 2812
-- Latin letters are capitalized and {|}~ are mapped to [\]^
ircFoldCase :: ByteString -> ByteArray
ircFoldCase bs = runST $
  do let n = B.length bs
     a <- BA.newByteArray n
     for_ [0..n-1] $ \i ->
       BA.writeByteArray a i (casemap PV.! fromIntegral (B.index bs i))
     BA.unsafeFreezeByteArray a

casemap :: PV.Vector Word8
casemap
  = PV.fromList
  $ map (fromIntegral . ord)
  $ ['\x00'..'`'] ++ "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^" ++ ['\x7f'..'\xff']
