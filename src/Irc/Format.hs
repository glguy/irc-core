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
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.CaseInsensitive (CI)
import Data.Monoid
import Data.String
import Data.Text (Text)
import System.IO (Handle)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
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
newtype Identifier = Identifier (CI ByteString)
  deriving (Read, Show, Eq, Ord)

instance IsString Identifier where
  fromString = Identifier . fromString

-- | Construct an 'Identifier' from a 'ByteString'
mkId :: ByteString -> Identifier
mkId = Identifier . CI.mk

idBytes :: Identifier -> ByteString
idBytes (Identifier x) = CI.original x

idDenote :: Identifier -> ByteString
idDenote (Identifier x) = CI.foldedCase x

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
