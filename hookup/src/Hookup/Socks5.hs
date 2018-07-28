{-# Language PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -Wno-missing-pattern-synonym-signatures #-}
{-|
Module      : Hookup.Socks5
Description : SOCKS5 network protocol implementation
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

This module provides types, parsers, and builders for the messages
used in the SOCKS5 protocol. See <https://tools.ietf.org/html/rfc1928>
-}
module Hookup.Socks5

  ( -- * Client hello message
    ClientHello(..)
  , buildClientHello
  , parseClientHello

  -- * Server hello message
  , ServerHello(..)
  , buildServerHello
  , parseServerHello

  -- * Command request message
  , Request(..)
  , buildRequest
  , parseRequest

  -- * Command response message
  , Response(..)
  , buildResponse
  , parseResponse

  -- * Network address types
  , Address(..)
  , Host(..)

  -- * Authentication methods
  , AuthMethod
      ( AuthNoAuthenticationRequired
      , AuthGssApi
      , AuthUsernamePassword
      , AuthNoAcceptableMethods )

  -- * Commands
  , Command
      ( Connect
      , Bind
      , UdpAssociate )

  -- * Command reply codes
  , Reply
      ( Reply
      , Succeeded
      , GeneralFailure
      , NotAllowed
      , NetUnreachable
      , HostUnreachable
      , ConnectionRefused
      , TTLExpired
      , CmdNotSupported
      , AddrNotSupported )

  )
  where

import           Control.Monad              (replicateM)
import           Data.Attoparsec.ByteString (Parser)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Builder    (Builder)
import           Data.Word                  (Word8, Word16)
import           Network.Socket             (HostAddress, HostAddress6, PortNumber,
                                             hostAddressToTuple, hostAddress6ToTuple,
                                             tupleToHostAddress, tupleToHostAddress6)
import qualified Data.Attoparsec.ByteString as Parser
import qualified Data.ByteString            as B
import qualified Data.ByteString.Builder    as Builder
import qualified Data.ByteString.Lazy       as L

-- | Authentication methods
newtype AuthMethod                      = AuthMethod Word8 deriving (Eq, Show)
pattern AuthNoAuthenticationRequired    = AuthMethod 0x00
pattern AuthGssApi                      = AuthMethod 0x01
pattern AuthUsernamePassword            = AuthMethod 0x02
pattern AuthNoAcceptableMethods         = AuthMethod 0xFF

-- | SOCKS client commands
newtype Command                         = Command Word8 deriving (Eq, Show)
pattern Connect                         = Command 1
pattern Bind                            = Command 2
pattern UdpAssociate                    = Command 3

-- | Tags used in the protocol messages for encoded 'Host' values
newtype HostTag                         = HostTag Word8 deriving (Eq, Show)
pattern IPv4Tag                         = HostTag 1
pattern DomainNameTag                   = HostTag 3
pattern IPv6Tag                         = HostTag 4

-- | Command reply codes
newtype Reply                           = Reply Word8 deriving (Eq, Show)
pattern Succeeded                       = Reply 0
pattern GeneralFailure                  = Reply 1
pattern NotAllowed                      = Reply 2
pattern NetUnreachable                  = Reply 3
pattern HostUnreachable                 = Reply 4
pattern ConnectionRefused               = Reply 5
pattern TTLExpired                      = Reply 6
pattern CmdNotSupported                 = Reply 7
pattern AddrNotSupported                = Reply 8

-- | Network host and port number
data Address = Address Host PortNumber
  deriving Show

-- | Network host identified by address or domain name.
data Host
  = IPv4 HostAddress      -- ^ IPv4 host address
  | IPv6 HostAddress6     -- ^ IPv6 host address
  | DomainName ByteString -- ^ Domain name (maximum length 255)
  deriving Show


-- | Initial SOCKS sent by client with proposed list of authentication methods.
data ClientHello = ClientHello
  { cHelloMethods :: [AuthMethod] -- ^ proposed methods (maximum length 255)
  }
  deriving Show

-- | Initial SOCKS sent by server with chosen authentication method.
data ServerHello = ServerHello
  { sHelloMethod  :: AuthMethod
  }
  deriving Show

-- | Client message used to request a network operation from the SOCKS server.
data Request = Request
  { reqCommand :: Command
  , reqAddress :: Address
  }
  deriving Show

-- | Server message used to indicate result of client's request.
data Response = Response
  { rspReply   :: Reply
  , rspAddress :: Address
  }
  deriving Show

-- | Transform a 'Builder' into a strict 'ByteString'
runBuilder :: Builder -> ByteString
runBuilder = L.toStrict . Builder.toLazyByteString

------------------------------------------------------------------------

buildCommand :: Command -> Builder
buildCommand (Command c) = Builder.word8 c

parseCommand :: Parser Command
parseCommand = Command <$> Parser.anyWord8

------------------------------------------------------------------------

buildHost :: Host -> Builder
buildHost (IPv4 hostAddr) = buildHostTag IPv4Tag       <> buildHostAddress  hostAddr
buildHost (IPv6 hostAddr) = buildHostTag IPv6Tag       <> buildHostAddress6 hostAddr
buildHost (DomainName dn) = buildHostTag DomainNameTag <> buildDomainName dn

parseHost :: Parser Host
parseHost =
  do tag <- parseHostTag
     case tag of
       IPv4Tag       -> IPv4       <$> parseHostAddress
       IPv6Tag       -> IPv6       <$> parseHostAddress6
       DomainNameTag -> DomainName <$> parseDomainName
       _             -> fail "bad address tag"

------------------------------------------------------------------------

buildAddress :: Address -> Builder
buildAddress (Address host port) = buildHost host <> buildPort port

parseAddress :: Parser Address
parseAddress = Address <$> parseHost <*> parsePort

------------------------------------------------------------------------

buildHostTag :: HostTag -> Builder
buildHostTag (HostTag tag) = Builder.word8 tag

parseHostTag :: Parser HostTag
parseHostTag = HostTag <$> Parser.anyWord8

------------------------------------------------------------------------

buildHostAddress :: HostAddress -> Builder
buildHostAddress hostAddr =
  case hostAddressToTuple hostAddr of
    (a1,a2,a3,a4) -> foldMap Builder.word8 [a1,a2,a3,a4]

parseHostAddress :: Parser HostAddress
parseHostAddress =
  do [a1,a2,a3,a4] <- replicateM 4 Parser.anyWord8
     return $! tupleToHostAddress (a1,a2,a3,a4)

------------------------------------------------------------------------

buildHostAddress6 :: HostAddress6 -> Builder
buildHostAddress6 hostAddr =
  case hostAddress6ToTuple hostAddr of
    (a1,a2,a3,a4,a5,a6,a7,a8) ->
      foldMap Builder.word16BE [a1,a2,a3,a4,a5,a6,a7,a8]

parseHostAddress6 :: Parser HostAddress6
parseHostAddress6 =
  do [a1,a2,a3,a4,a5,a6,a7,a8] <- replicateM 8 parseWord16BE
     return $! tupleToHostAddress6 (a1,a2,a3,a4,a5,a6,a7,a8)

------------------------------------------------------------------------

buildDomainName :: ByteString -> Builder
buildDomainName bs
  | B.length bs < 256 = Builder.word8 (fromIntegral (B.length bs)) <>
                        Builder.byteString bs
  | otherwise = error "SOCKS5 domain name too long"

parseDomainName :: Parser ByteString
parseDomainName =
  do len <- Parser.anyWord8
     Parser.take (fromIntegral len)

------------------------------------------------------------------------

buildPort :: PortNumber -> Builder
buildPort port = Builder.word16BE (fromIntegral port)

parsePort :: Parser PortNumber
parsePort = fromIntegral <$> parseWord16BE

------------------------------------------------------------------------

buildVersion :: Builder
buildVersion = Builder.word8 5

parseVersion :: Parser ()
parseVersion = () <$ Parser.word8 5

------------------------------------------------------------------------

buildAuthMethod :: AuthMethod -> Builder
buildAuthMethod (AuthMethod x) = Builder.word8 x

parseAuthMethod :: Parser AuthMethod
parseAuthMethod = AuthMethod <$> Parser.anyWord8

------------------------------------------------------------------------

buildReply :: Reply -> Builder
buildReply (Reply x) = Builder.word8 x

parseReply :: Parser Reply
parseReply = Reply <$> Parser.anyWord8

------------------------------------------------------------------------

buildReserved :: Builder
buildReserved = Builder.word8 0

parseReserved :: Parser ()
parseReserved = () <$ Parser.anyWord8

------------------------------------------------------------------------

-- | Build a list of buildable things prefixing the length of the list
-- as a single byte. The list must not be longer than 255 elements.
buildListOf :: (a -> Builder) -> [a] -> Builder
buildListOf builder xs
  | length xs < 256 = Builder.word8 (fromIntegral (length xs)) <>
                      foldMap builder xs
  | otherwise       = error "buildListOf: list too long"

-- | Parse a list of parsable things where the length of the list
-- is encoded as a single byte before the items to be parsed.
parseListOf :: Parser a -> Parser [a]
parseListOf parser =
  do n <- Parser.anyWord8
     replicateM (fromIntegral n) parser

------------------------------------------------------------------------

buildClientHello :: ClientHello -> ByteString
buildClientHello msg =
  runBuilder $
  buildVersion <>
  buildListOf buildAuthMethod (cHelloMethods msg)

parseClientHello :: Parser ClientHello
parseClientHello =
  ClientHello
    <$  parseVersion
    <*> parseListOf parseAuthMethod

------------------------------------------------------------------------

buildServerHello :: ServerHello -> ByteString
buildServerHello msg =
  runBuilder $
  buildVersion <>
  buildAuthMethod (sHelloMethod msg)

parseServerHello :: Parser ServerHello
parseServerHello =
  ServerHello
    <$  parseVersion
    <*> parseAuthMethod

------------------------------------------------------------------------

buildRequest :: Request -> ByteString
buildRequest req =
  runBuilder $
  buildVersion                    <>
  buildCommand  (reqCommand  req) <>
  buildReserved                   <>
  buildAddress  (reqAddress  req)

parseRequest :: Parser Request
parseRequest =
  Request
    <$  parseVersion
    <*> parseCommand
    <*  parseReserved
    <*> parseAddress

------------------------------------------------------------------------

buildResponse :: Response -> ByteString
buildResponse msg =
  runBuilder $
  buildVersion                  <>
  buildReply   (rspReply   msg) <>
  buildReserved                 <>
  buildAddress (rspAddress msg)

parseResponse :: Parser Response
parseResponse =
  Response
    <$  parseVersion
    <*> parseReply
    <*  parseReserved
    <*> parseAddress

------------------------------------------------------------------------

-- | Match a 16-bit, big-endian word.
parseWord16BE :: Parser Word16
parseWord16BE =
  do hi <- Parser.anyWord8
     lo <- Parser.anyWord8
     return $! fromIntegral hi * 0x100 + fromIntegral lo
