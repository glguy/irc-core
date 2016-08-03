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
  ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Function
import           Data.Hashable         (Hashable (hashWithSalt))
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as Text

-- | Identifier representing channels and nicknames
data Identifier = Identifier Text ByteString
  deriving (Read, Show)

-- | Equality on normalized identifier
instance Eq Identifier where
  (==) = (==) `on` idDenote

-- | Comparison on normalized identifier
instance Ord Identifier where
  compare = compare `on` idDenote

-- | Hash on normalized identifier
instance Hashable Identifier where
  hashWithSalt s = hashWithSalt s . idDenote

-- | Construct an 'Identifier' from a 'ByteString'
mkId :: Text -> Identifier
mkId x = Identifier x (ircFoldCase (Text.encodeUtf8 x))

-- | Returns the original 'Text' of an 'Identifier'
idText :: Identifier -> Text
idText (Identifier x _) = x

-- | Returns the case-normalized 'ByteString' of an 'Identifier'
-- which is suitable for comparison or hashing.
idDenote :: Identifier -> ByteString
idDenote (Identifier _ x) = x

-- | Capitalize a string according to RFC 2812
-- Latin letters are capitalized and {|}~ are mapped to [\]^
ircFoldCase :: ByteString -> ByteString
ircFoldCase = B.map (B.index casemap . fromIntegral)

casemap :: ByteString
casemap
  = B8.pack
  $ ['\x00'..'`'] ++ "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^" ++ ['\x7f'..'\xff']
