module Irc.Identifier
  ( Identifier
  , idDenote
  , mkId
  , idText
  ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Hashable         (Hashable (hashWithSalt))
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as Text

-- | Case insensitive identifier representing channels and nicknames
data Identifier = Identifier Text ByteString
  deriving (Read, Show)

-- Equality on normalized 'Identifiers'
instance Eq Identifier where
  x == y = idDenote x == idDenote y

-- Comparison on normalized 'Identifiers'
instance Ord Identifier where
  compare x y = compare (idDenote x) (idDenote y)

instance Hashable Identifier where
  hashWithSalt s = hashWithSalt s . idDenote

-- | Construct an 'Identifier' from a 'ByteString'
mkId :: Text -> Identifier
mkId x = Identifier x (ircFoldCase (Text.encodeUtf8 x))

-- | Returns the original 'Text' of an 'Identifier'
idText :: Identifier -> Text
idText (Identifier x _) = x

-- | Returns the case-normalized 'ByteString' of an 'Identifier'
-- which is suitable for comparison.
idDenote :: Identifier -> ByteString
idDenote (Identifier _ x) = x

-- | Capitalize a string according to RFC 2812
-- Latin letters are capitalized and {|}~ are mapped to [\]^
ircFoldCase :: ByteString -> ByteString
ircFoldCase = B.map (B.index casemap . fromIntegral)

casemap :: ByteString
casemap = B8.pack
          "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\
          \\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\
          \ !\"#$%&'()*+,-./0123456789:;<=>?\
          \@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_\
          \`ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^\x7f\
          \\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8b\x8c\x8d\x8e\x8f\
          \\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f\
          \\xa0\xa1\xa2\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xad\xae\xaf\
          \\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc\xbd\xbe\xbf\
          \\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9\xca\xcb\xcc\xcd\xce\xcf\
          \\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf\
          \\xe0\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef\
          \\xf0\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff"
