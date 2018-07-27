{-# Language PatternSynonyms #-}
module Hookup.Socks5 where

import Control.Monad (replicateM)
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Word
import Network.Socket
import qualified Data.Attoparsec.ByteString as Parser
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as L

newtype AuthMethod                   = AuthMethod Word8 deriving (Eq, Show)
pattern AuthNoAuthenticationRequired = AuthMethod 0x00
pattern AuthGssApi                   = AuthMethod 0x01
pattern AuthUsernamePassword         = AuthMethod 0x02
pattern AuthNoAcceptableMethods      = AuthMethod 0xFF

newtype Version         = Version Word8 deriving (Eq, Show)
pattern Version5        = Version 5

newtype Command         = Command Word8 deriving (Eq, Show)
pattern Connect         = Command 1
pattern Bind            = Command 2
pattern UdpAssociate    = Command 3

newtype AddressTag      = AddressTag Word8 deriving (Eq, Show)
pattern IPv4Tag         = AddressTag 1
pattern DomainNameTag   = AddressTag 3
pattern IPv6Tag         = AddressTag 4

newtype Reply           = Reply Word8 deriving (Eq, Show)
pattern Succeeded       = Reply 0
pattern GeneralFailure  = Reply 1
pattern NotAllowed      = Reply 2
pattern NetUnreachable  = Reply 3
pattern HostUnreachable = Reply 4
pattern ConnectionRefused = Reply 5
pattern TTLExpired      = Reply 6
pattern CmdNotSupported = Reply 7
pattern AddrNotSupported = Reply 8

newtype Reserved        = Reserved Word8 deriving (Eq, Show)
reserved0               = Reserved 0

data Address
  = IPv4 HostAddress
  | IPv6 HostAddress6
  | DomainName ByteString
  deriving Show


data ClientHello = ClientHello
  { cHelloVersion :: Version
  , cHelloMethods :: [AuthMethod]
  }
  deriving Show

data ServerHello = ServerHello
  { sHelloVersion :: Version
  , sHelloMethod :: !AuthMethod
  }
  deriving Show

data Request = Request
  { reqVerion  :: Version
  , reqCommand :: Command
  , reqReserved :: Reserved
  , reqAddress :: Address
  , reqPort    :: PortNumber
  }
  deriving Show

data Response = Response
  { rspVersion :: Version
  , rspReply :: Reply
  , rspReserved :: Reserved
  , rspAddress :: Address
  , rspPort    :: PortNumber
  }
  deriving Show

runBuilder :: Builder -> ByteString
runBuilder = L.toStrict . Builder.toLazyByteString

buildVersion :: Version -> Builder
buildVersion (Version v) = Builder.word8 v

buildCommand :: Command -> Builder
buildCommand (Command c) = Builder.word8 c

buildAddress :: Address -> Builder
buildAddress (IPv4 hostAddr) = buildAddressTag IPv4Tag <> buildHostAddress  hostAddr
buildAddress (IPv6 hostAddr) = buildAddressTag IPv6Tag <> buildHostAddress6 hostAddr
buildAddress (DomainName dn) = buildAddressTag DomainNameTag <> buildDomainName dn

parseAddress :: Parser Address
parseAddress =
  do tag <- parseAddressTag
     case tag of
       IPv4Tag       -> IPv4       <$> parseHostAddress
       IPv6Tag       -> IPv6       <$> parseHostAddress6
       DomainNameTag -> DomainName <$> parseDomainName
       _             -> fail "bad address tag"

buildAddressTag :: AddressTag -> Builder
buildAddressTag (AddressTag tag) = Builder.word8 tag

parseAddressTag :: Parser AddressTag
parseAddressTag = AddressTag <$> Parser.anyWord8

buildHostAddress :: HostAddress -> Builder
buildHostAddress hostAddr =
  case hostAddressToTuple hostAddr of
    (a1,a2,a3,a4) -> foldMap Builder.word8 [a1,a2,a3,a4]

parseHostAddress :: Parser HostAddress
parseHostAddress =
  do [a1,a2,a3,a4] <- replicateM 4 Parser.anyWord8
     return $! tupleToHostAddress (a1,a2,a3,a4)

buildHostAddress6 :: HostAddress6 -> Builder
buildHostAddress6 hostAddr =
  case hostAddress6ToTuple hostAddr of
    (a1,a2,a3,a4,a5,a6,a7,a8) ->
      foldMap Builder.word16BE [a1,a2,a3,a4,a5,a6,a7,a8]

parseHostAddress6 :: Parser HostAddress6
parseHostAddress6 =
  do [a1,a2,a3,a4,a5,a6,a7,a8] <- replicateM 8 parseWord16BE
     return $! tupleToHostAddress6 (a1,a2,a3,a4,a5,a6,a7,a8)

buildDomainName :: ByteString -> Builder
buildDomainName bs
  | B.length bs > 255 = error "SOCKS5 domain name too long"
  | otherwise = Builder.word8 (fromIntegral (B.length bs)) <>
                Builder.byteString bs

parseDomainName :: Parser ByteString
parseDomainName =
  do len <- Parser.anyWord8
     Parser.take (fromIntegral len)

buildPort :: PortNumber -> Builder
buildPort port = Builder.word16BE (fromIntegral port)

parsePort :: Parser PortNumber
parsePort = fromIntegral <$> parseWord16BE

parseWord16BE :: Parser Word16
parseWord16BE =
  do hi <- Parser.anyWord8
     lo <- Parser.anyWord8
     return $! fromIntegral hi * 256 + fromIntegral lo

buildClientHello :: ClientHello -> ByteString
buildClientHello msg =
  runBuilder $
  buildVersion (cHelloVersion msg) <>
  Builder.word8 (fromIntegral (length (cHelloMethods msg))) <>
  foldMap (\(AuthMethod i) -> Builder.word8 i) (cHelloMethods msg)

parseVersion :: Parser Version
parseVersion = Version <$> Parser.anyWord8

parseAuthMethod :: Parser AuthMethod
parseAuthMethod = AuthMethod <$> Parser.anyWord8

parseReply :: Parser Reply
parseReply = Reply <$> Parser.anyWord8

buildReserved :: Reserved -> Builder
buildReserved (Reserved x) = Builder.word8 x

parseReserved :: Parser Reserved
parseReserved = Reserved <$> Parser.anyWord8

parseServerHello :: Parser ServerHello
parseServerHello =
  ServerHello
    <$> parseVersion
    <*> parseAuthMethod

buildRequest :: Request -> ByteString
buildRequest req =
  runBuilder $
  buildVersion  (reqVerion   req) <>
  buildCommand  (reqCommand  req) <>
  buildReserved (reqReserved req) <>
  buildAddress  (reqAddress  req) <>
  buildPort     (reqPort     req)

parseResponse :: Parser Response
parseResponse =
  Response
    <$> parseVersion
    <*> parseReply
    <*> parseReserved
    <*> parseAddress
    <*> parsePort
