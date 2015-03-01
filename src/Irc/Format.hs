-- | This module provides a parser and printer for the low-level IRC
-- message format.
module Irc.Format (UserInfo(..), RawIrcMsg(..), parseRawIrcMsg, renderRawIrcMsg) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as L

data UserInfo = UserInfo
  { userNick :: ByteString
  , userName :: Maybe ByteString
  , userHost :: Maybe ByteString
  }
  deriving (Read, Show)

data RawIrcMsg = RawIrcMsg
  { msgPrefix  :: Maybe UserInfo
  , msgCommand :: ByteString
  , msgParams  :: [ByteString]
  }
  deriving (Read, Show)

parseRawIrcMsg :: ByteString -> Maybe RawIrcMsg
parseRawIrcMsg x =
  do (y,ys) <- B.uncons x
     if y == 58
       then case duplicate (tokens ys) of
              prefix : command : rest ->
                return $! RawIrcMsg (Just (parsePrefix prefix))
                                    command
                                    rest
              _ -> Nothing
       else case duplicate (tokens x) of
              command : rest ->
                return $! RawIrcMsg Nothing command rest
              _ -> Nothing

tokens :: ByteString -> [ByteString]
tokens x =
  case B.uncons x of
    Nothing -> []
    Just (58,xs) -> [xs]
    _            -> let (a,b) = B.break (==32) x
                    in a : tokens (B.drop 1 b)

parsePrefix :: ByteString -> UserInfo
parsePrefix x = UserInfo
  { userNick = nick
  , userName = if B.null user then Nothing else Just (B.drop 1 user)
  , userHost = if B.null host then Nothing else Just (B.drop 1 host)
  }
  where
  (nickuser,host) = B.break (==64) x
  (nick,user) = B.break (==33) nickuser

renderRawIrcMsg :: RawIrcMsg -> ByteString
renderRawIrcMsg m = L.toStrict $ Builder.toLazyByteString $
  Builder.byteString (msgCommand m)
  <> buildParams (msgParams m)
  <> Builder.word8 13
  <> Builder.word8 10

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
