{-# LANGUAGE OverloadedStrings #-}
-- | This module provides a parser and printer for the low-level IRC
-- message format.
module Irc.Format
  ( UserInfo(..)
  , RawIrcMsg(..)
  , parseRawIrcMsg
  , renderRawIrcMsg
  , parseUserInfo
  , renderUserInfo
  , ircGetLine
  , Identifier
  , mkId
  , idBytes
  , idDenote
  , asUtf8
  , ircFoldCase
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Monoid
import Data.String
import Data.Text (Text)
import System.IO (Handle)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

-- | 'UserInfo' packages a nickname along with the username and hsotname
-- if they are known in the current context.
data UserInfo = UserInfo
  { userNick :: Identifier
  , userName :: Maybe ByteString
  , userHost :: Maybe ByteString
  }
  deriving (Read, Show)

-- | 'RawIrcMsg' breaks down the IRC protocol into its most basic parts.
-- The "trailing" parameter indicated in the IRC protocol with a leading
-- colon will appear as the last parameter in the parameter list.
--
-- @:prefix COMMAND param0 param1 param2 .. paramN@
data RawIrcMsg = RawIrcMsg
  { msgPrefix  :: Maybe UserInfo
  , msgCommand :: ByteString
  , msgParams  :: [ByteString]
  }
  deriving (Read, Show)

-- | Case insensitive identifier representing channels and nicknames
data Identifier = Identifier ByteString ByteString
  deriving (Read, Show, Eq, Ord)

instance IsString Identifier where
  fromString = mkId . fromString

-- | Construct an 'Identifier' from a 'ByteString'
mkId :: ByteString -> Identifier
mkId x = Identifier x (ircFoldCase x)

idBytes :: Identifier -> ByteString
idBytes (Identifier x _) = x

idDenote :: Identifier -> ByteString
idDenote (Identifier _ x) = x

-- | Attempt to split an IRC protocol message without its trailing newline
-- information into a structured message.
parseRawIrcMsg :: ByteString -> Maybe RawIrcMsg
parseRawIrcMsg x =
  do (y,ys) <- B.uncons x
     if y == 58
       then case duplicate (tokens ys) of
              prefix : command : rest ->
                return $! RawIrcMsg (Just (parseUserInfo prefix))
                                    command
                                    rest
              _ -> Nothing
       else case duplicate (tokens x) of
              command : rest ->
                return $! RawIrcMsg Nothing command rest
              _ -> Nothing

renderUserInfo :: UserInfo -> ByteString
renderUserInfo u = idBytes (userNick u)
                <> maybe B.empty ("!" <>) (userName u)
                <> maybe B.empty ("@" <>) (userHost u)

-- | Split up a bytestring into space delimited tokens. The last token
-- in the bytestring is potentially indicated by a leading colon.
tokens :: ByteString -> [ByteString]
tokens x =
  case B.uncons x of
    Nothing -> []
    Just (58,xs) -> [xs]
    _            -> let (a,b) = B.break (==32) x
                    in a : tokens (B.drop 1 b)

-- | Split up a hostmask into a nickname, username, and hostname.
-- The username and hostname might not be defined but are delimited by
-- a @!@ and @\@@ respectively.
parseUserInfo :: ByteString -> UserInfo
parseUserInfo x = UserInfo
  { userNick = mkId nick
  , userName = if B.null user then Nothing else Just (B.drop 1 user)
  , userHost = if B.null host then Nothing else Just (B.drop 1 host)
  }
  where
  (nickuser,host) = B.break (==64) x
  (nick,user) = B.break (==33) nickuser

-- | Serialize a structured IRC protocol message back into its wire
-- format. This command adds the required trailing newline.
renderRawIrcMsg :: RawIrcMsg -> ByteString
renderRawIrcMsg m = L.toStrict $ Builder.toLazyByteString $
  Builder.byteString (msgCommand m)
  <> buildParams (msgParams m)
  <> Builder.word8 13
  <> Builder.word8 10

-- | Build concatenate a list of parameters into a single, space-
-- delimited bytestring. Use a colon for the last parameter if it contains
-- a colon or a space.
buildParams :: [ByteString] -> Builder
buildParams [x]
  | B.elem 32 x || B.elem 58 x
  = Builder.word8 32 <> Builder.word8 58 <> Builder.byteString x
buildParams (x:xs)
  = Builder.word8 32 <> Builder.byteString x <> buildParams xs
buildParams [] = mempty

-- | Copy all of the bytestrings strictly to make sure
-- we're not holding on to large packets.
duplicate :: [ByteString] -> [ByteString]
duplicate [] = []
duplicate (x:xs) = ((:) $! B.copy x) $! duplicate xs

ircGetLine :: Handle -> IO ByteString
ircGetLine h =
  do b <- B.hGetLine h
     return $! if not (B.null b) && B8.last b == '\r' then B.init b else b

asUtf8 :: ByteString -> Text
asUtf8 = Text.decodeUtf8With Text.lenientDecode

-- | Capitalize a string according to RFC 2812
-- Latin letters are capitalized and {|}~ are mapped to [\]^
ircFoldCase :: ByteString -> ByteString
ircFoldCase = B.map (B.index casemap . fromIntegral)

casemap :: ByteString
casemap = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0a\x0b\x0c\x0d\x0e\x0f\
          \\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1a\x1b\x1c\x1d\x1e\x1f\
          \ !\"#$%&'()*+,-./0123456789:;<=>?\
          \@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_\
          \`ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^\x7f\
          \\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8a\x8b\x8c\x8d\x8e\x8f\
          \\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9a\x9a\x9b\x9c\x9d\x9e\x9f\
          \\xa0\xa1\xa2\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xaa\xab\xac\xad\xae\xaf\
          \\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xba\xbb\xbc\xbd\xbe\xbf\
          \\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9\xca\xca\xcb\xcc\xcd\xce\xcf\
          \\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9\xda\xda\xdb\xdc\xdd\xde\xdf\
          \\xe0\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9\xea\xea\xeb\xec\xed\xee\xef\
          \\xf0\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfa\xfb\xfc\xfd\xfe\xff"

