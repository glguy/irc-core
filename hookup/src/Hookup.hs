{-|
Module      : Hookup
Description : Network connections generalized over TLS and SOCKS
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a uniform interface to network connections
with optional support for TLS and SOCKS.
-}
module Hookup
  (
  -- * Configuration
  ConnectionParams(..),
  SocksParams(..),
  TlsParams(..),
  defaultFamily,
  defaultTlsParams,

  -- * Connections
  Connection,
  connect,
  connectWithSocket,
  recv,
  recvLine,
  send,
  close,
  putBuf,

  -- * Errors
  ConnectionFailure(..),
  CommandReply(..)
  ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Foldable
import           Data.List (intercalate)
import           Network.Socket (Socket, AddrInfo, PortNumber, HostName, Family)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketB
import           OpenSSL.Session (SSL, SSLContext)
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509 as SSL
import           OpenSSL.X509.SystemStore
import qualified OpenSSL.PEM as PEM
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as Parser

import           Hookup.OpenSSL (installVerification)
import           Hookup.Socks5


-- | Parameters for 'connect'.
--
-- Common defaults for fields: 'defaultFamily', 'defaultTlsParams'
--
-- The address family can be specified in order to force only
-- IPv4 or IPv6 to be used. The default behavior is to support both.
-- It can be useful to specify exactly one of these in the case that
-- the other is misconfigured and a hostname is resolving to both.
--
-- When a 'SocksParams' is provided the connection will be established
-- using a SOCKS5 proxy.
--
-- When a 'TlsParams' is provided the connection negotiate TLS at connect
-- time in order to protect the stream.
data ConnectionParams = ConnectionParams
  { cpFamily :: Family           -- ^ IP Protocol family (default 'AF_UNSPEC')
  , cpHost  :: HostName          -- ^ Destination host
  , cpPort  :: PortNumber        -- ^ Destination TCP port
  , cpSocks :: Maybe SocksParams -- ^ Optional SOCKS5 parameters
  , cpTls   :: Maybe TlsParams   -- ^ Optional TLS parameters
  }


-- | SOCKS5 connection parameters
data SocksParams = SocksParams
  { spHost :: HostName   -- ^ SOCKS server host
  , spPort :: PortNumber -- ^ SOCKS server port
  }


-- | TLS connection parameters. These parameters are passed to
-- OpenSSL when making a secure connection.
data TlsParams = TlsParams
  { tpClientCertificate  :: Maybe FilePath -- ^ Path to client certificate
  , tpClientPrivateKey   :: Maybe FilePath -- ^ Path to client private key
  , tpServerCertificate  :: Maybe FilePath -- ^ Path to CA certificate bundle
  , tpCipherSuite        :: String -- ^ OpenSSL cipher suite name (e.g. "HIGH")
  , tpInsecure           :: Bool -- ^ Disables certificate checking when 'True'
  }

-- | Type for errors that can be thrown by this package.
data ConnectionFailure
  -- | Failure during 'getAddrInfo' resolving remote host
  = HostnameResolutionFailure IOError
  -- | Failure during 'connect' to remote host
  | ConnectionFailure [IOError]
  -- | Failure during 'recvLine'
  | LineTooLong
  -- | Incomplete line during 'recvLine'
  | LineTruncated
  -- | Socks command rejected by server by given reply code
  | SocksError CommandReply
  -- | Socks authentication method was not accepted
  | SocksAuthenticationError
  -- | Socks server sent an invalid message or no message.
  | SocksProtocolError
  -- | Domain name was too long for SOCKS protocol
  | SocksBadDomainName
  deriving Show

-- | 'displayException' implemented for prettier messages
instance Exception ConnectionFailure where
  displayException LineTruncated = "connection closed while reading line"
  displayException LineTooLong   = "line length exceeded maximum"
  displayException (ConnectionFailure xs) =
    "connection attempt failed due to: " ++
      intercalate ", " (map displayException xs)
  displayException (HostnameResolutionFailure x) =
    "hostname resolution failed: " ++ displayException x
  displayException SocksAuthenticationError =
    "SOCKS authentication method rejected"
  displayException SocksProtocolError =
    "SOCKS server protocol error"
  displayException SocksBadDomainName =
    "SOCKS domain name length limit exceeded"
  displayException (SocksError reply) =
    "SOCKS command rejected: " ++
    case reply of
      Succeeded         -> "succeeded"
      GeneralFailure    -> "general SOCKS server failure"
      NotAllowed        -> "connection not allowed by ruleset"
      NetUnreachable    -> "network unreachable"
      HostUnreachable   -> "host unreachable"
      ConnectionRefused -> "connection refused"
      TTLExpired        -> "TTL expired"
      CmdNotSupported   -> "command not supported"
      AddrNotSupported  -> "address type not supported"
      CommandReply n    -> "unknown reply " ++ show n

-- | Default 'Family' value is unspecified and allows both INET and INET6.
defaultFamily :: Socket.Family
defaultFamily = Socket.AF_UNSPEC

-- | Default values for TLS that use no client certificates, use
-- system CA root, @HIGH@ cipher suite, and which validate hostnames.
defaultTlsParams :: TlsParams
defaultTlsParams = TlsParams
  { tpClientCertificate  = Nothing
  , tpClientPrivateKey   = Nothing
  , tpServerCertificate  = Nothing -- use system provided CAs
  , tpCipherSuite        = "HIGH"
  , tpInsecure           = False
  }

------------------------------------------------------------------------
-- Opening sockets
------------------------------------------------------------------------

-- | Open a socket using the given parameters either directly or
-- via a SOCKS server.
openSocket :: ConnectionParams -> IO Socket
openSocket params =
  case cpSocks params of
    Nothing -> openSocket' (cpFamily params) (cpHost params) (cpPort params)
    Just sp ->
      do sock <- openSocket' (cpFamily params) (spHost sp) (spPort sp)
         (sock <$ socksConnect sock (cpHost params) (cpPort params))
           `onException` Socket.close sock


netParse :: Show a => Socket -> Parser a -> IO a
netParse sock parser =
  do -- receiving 1 byte at a time is not efficient, but these messages
     -- are very short and we don't want to read any more from the socket
     -- than is necessary
     result <- Parser.parseWith
                 (SocketB.recv sock 1)
                 parser
                 B.empty
     case result of
       Parser.Done i x | B.null i -> return x
       _ -> throwIO SocksProtocolError


socksConnect :: Socket -> HostName -> PortNumber -> IO ()
socksConnect sock host port =
  do SocketB.sendAll sock $
       buildClientHello ClientHello
         { cHelloMethods = [AuthNoAuthenticationRequired] }

     validateHello =<< netParse sock parseServerHello

     let dnBytes = B8.pack host
     unless (B.length dnBytes < 256)
       (throwIO SocksBadDomainName)

     SocketB.sendAll sock $
       buildRequest Request
         { reqCommand  = Connect
         , reqAddress  = Address (DomainName dnBytes) port
         }

     validateResponse =<< netParse sock parseResponse


validateHello :: ServerHello -> IO ()
validateHello hello =
  unless (sHelloMethod hello == AuthNoAuthenticationRequired)
    (throwIO SocksAuthenticationError)

validateResponse :: Response -> IO ()
validateResponse response =
  unless (rspReply response == Succeeded )
    (throwIO (SocksError (rspReply response)))


openSocket' :: Family -> HostName -> PortNumber -> IO Socket
openSocket' family h p =
  do let hints = Socket.defaultHints
           { Socket.addrFamily     = family
           , Socket.addrSocketType = Socket.Stream
           , Socket.addrFlags      = [Socket.AI_ADDRCONFIG
                                     ,Socket.AI_NUMERICSERV]
           }
     res <- try (Socket.getAddrInfo (Just hints) (Just h) (Just (show p)))
     case res of
       Right ais -> attemptConnections [] ais
       Left  ioe -> throwIO (HostnameResolutionFailure ioe)


-- | Try establishing a connection to the services indicated by
-- a given list of 'AddrInfo' values. Either return a socket that
-- has successfully connected to one of the candidate 'AddrInfo's
-- or throw a 'ConnectionFailure' exception will all of the
-- encountered errors.
attemptConnections ::
  [IOError]         {- ^ accumulated errors  -} ->
  [Socket.AddrInfo] {- ^ candidate AddrInfos -} ->
  IO Socket         {- ^ connected socket    -}
attemptConnections exs [] = throwIO (ConnectionFailure exs)
attemptConnections exs (ai:ais) =
  do res <- try (connectToAddrInfo ai)
     case res of
       Left ex -> attemptConnections (ex:exs) ais
       Right s -> return s

-- | Create a socket and connect to the service identified
-- by the given 'AddrInfo' and return the connected socket.
connectToAddrInfo :: AddrInfo -> IO Socket
connectToAddrInfo info
  = bracketOnError (socket' info) Socket.close
  $ \s -> s <$ Socket.connect s (Socket.addrAddress info)

-- | Open a 'Socket' using the parameters from an 'AddrInfo'
socket' :: AddrInfo -> IO Socket
socket' ai =
  Socket.socket
    (Socket.addrFamily     ai)
    (Socket.addrSocketType ai)
    (Socket.addrProtocol   ai)


------------------------------------------------------------------------
-- Generalization of Socket
------------------------------------------------------------------------

data NetworkHandle = SSL SSL | Socket Socket


openNetworkHandle ::
  ConnectionParams {- ^ parameters             -} ->
  IO Socket        {- ^ socket creation action -} ->
  IO NetworkHandle {- ^ open network handle    -}
openNetworkHandle params mkSocket =
  case cpTls params of
    Nothing  -> Socket <$> mkSocket
    Just tls -> SSL <$> startTls tls (cpHost params) mkSocket


closeNetworkHandle :: NetworkHandle -> IO ()
closeNetworkHandle (Socket s) = Socket.close s
closeNetworkHandle (SSL s) =
  do SSL.shutdown s SSL.Unidirectional
     traverse_ Socket.close (SSL.sslSocket s)

networkSend :: NetworkHandle -> ByteString -> IO ()
networkSend (Socket s) = SocketB.sendAll s
networkSend (SSL    s) = SSL.write       s

networkRecv :: NetworkHandle -> Int -> IO ByteString
networkRecv (Socket s) = SocketB.recv s
networkRecv (SSL    s) = SSL.read     s


------------------------------------------------------------------------
-- Sockets with a receive buffer
------------------------------------------------------------------------

-- | A connection to a network service along with its read buffer
-- used for line-oriented protocols. The connection could be a plain
-- network connection, SOCKS connected, or TLS.
data Connection = Connection (MVar ByteString) NetworkHandle

-- | Open network connection to TCP service specified by
-- the given parameters.
--
-- The resulting connection MUST be closed with 'close' to avoid leaking
-- resources.
--
-- Throws 'IOError', 'SocksError', 'SSL.ProtocolError', 'ConnectionFailure'
connect ::
  ConnectionParams {- ^ parameters      -} ->
  IO Connection    {- ^ open connection -}
connect params =
  do h <- openNetworkHandle params (openSocket params)
     b <- newMVar B.empty
     return (Connection b h)

-- | Create a new 'Connection' using an already connected socket.
-- This will attempt to start TLS if configured but will ignore
-- any SOCKS server settings as it is assumed that the socket
-- is already actively connected to the intended service.
--
-- Throws 'SSL.ProtocolError'
connectWithSocket ::
  ConnectionParams {- ^ parameters       -} ->
  Socket           {- ^ connected socket -} ->
  IO Connection    {- ^ open connection  -}
connectWithSocket params sock =
  do h <- openNetworkHandle params (return sock)
     b <- newMVar B.empty
     return (Connection b h)

-- | Close network connection.
close ::
  Connection {- ^ open connection -} ->
  IO ()
close (Connection _ h) = closeNetworkHandle h

-- | Receive the next chunk from the stream. This operation will first
-- return the buffer if it contains a non-empty chunk. Otherwise it will
-- request up to the requested number of bytes from the stream.
recv ::
  Connection    {- ^ open connection              -} ->
  Int           {- ^ maximum underlying recv size -} ->
  IO ByteString {- ^ next chunk from stream       -}
recv (Connection buf h) n =
  do bufChunk <- swapMVar buf B.empty
     if B.null bufChunk
       then networkRecv h n
       else return bufChunk

-- | Receive a line from the network connection. Both
-- @"\\r\\n"@ and @"\\n"@ are recognized.
--
-- Returning 'Nothing' means that the peer has closed its half of
-- the connection.
--
-- Unterminated lines will raise a 'LineTruncated' exception. This
-- can happen if the peer transmits some data and closes its end
-- without transmitting a line terminator.
--
-- Throws: 'ConnectionAbruptlyTerminated', 'ConnectionFailure', 'IOError'
recvLine ::
  Connection            {- ^ open connection            -} ->
  Int                   {- ^ maximum line length        -} ->
  IO (Maybe ByteString) {- ^ next line or end-of-stream -}
recvLine (Connection buf h) n =
  modifyMVar buf $ \bs ->
    go (B.length bs) bs []
  where
    -- bsn: cached length of concatenation of (bs:bss)
    -- bs : most recent chunk
    -- bss: other chunks ordered from most to least recent
    go bsn bs bss =
      case B8.elemIndex '\n' bs of
        Just i -> return (B.tail b, -- tail drops newline
                          Just (cleanEnd (B.concat (reverse (a:bss)))))
          where
            (a,b) = B.splitAt i bs
        Nothing ->
          do when (bsn >= n) (throwIO LineTooLong)
             more <- networkRecv h n
             if B.null more -- connection closed
               then if bsn == 0 then return (B.empty, Nothing)
                                else throwIO LineTruncated
               else go (bsn + B.length more) more (bs:bss)


-- | Push a 'ByteString' onto the buffer so that it will be the first
-- bytes to be read on the next receive operation. This could perhaps
-- be useful for putting the unused portion of a 'recv' back into the
-- buffer for future 'recvLine' or 'recv' operations.
putBuf ::
  Connection {- ^ connection         -} ->
  ByteString {- ^ new head of buffer -} ->
  IO ()
putBuf (Connection buf h) bs =
  modifyMVar_ buf (\old -> return $! B.append bs old)


-- | Remove the trailing @'\\r'@ if one is found.
cleanEnd :: ByteString -> ByteString
cleanEnd bs
  | B.null bs || B8.last bs /= '\r' = bs
  | otherwise                       = B.init bs


-- | Send bytes on the network connection. This ensures the whole chunk is
-- transmitted, which might take multiple underlying sends.
--
-- Throws: 'IOError', 'ProtocolError'
send ::
  Connection {- ^ open connection -} ->
  ByteString {- ^ chunk           -} ->
  IO ()
send (Connection _ h) = networkSend h


------------------------------------------------------------------------


-- | Initiate a TLS session on the given socket destined for
-- the given hostname. When successful an active TLS connection
-- is returned with certificate verification successful when
-- requested. This function requires that the TLSParams component
-- of 'ConnectionParams' is set.
startTls ::
  TlsParams {- ^ connection params      -} ->
  String    {- ^ hostname               -} ->
  IO Socket {- ^ socket creation action -} ->
  IO SSL    {- ^ connected TLS          -}
startTls tp hostname mkSocket = SSL.withOpenSSL $
  do ctx <- SSL.context

     -- configure context
     SSL.contextSetCiphers          ctx (tpCipherSuite tp)
     installVerification            ctx hostname
     SSL.contextSetVerificationMode ctx (verificationMode (tpInsecure tp))
     SSL.contextAddOption           ctx SSL.SSL_OP_ALL
     SSL.contextRemoveOption        ctx SSL.SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS

     -- configure certificates
     setupCaCertificates ctx          (tpServerCertificate tp)
     traverse_ (setupCertificate ctx) (tpClientCertificate tp)
     traverse_ (setupPrivateKey  ctx) (tpClientPrivateKey  tp)

     -- add socket to context
     -- creation of the socket is delayed until this point to avoid
     -- leaking the file descriptor in the cases of exceptions above.
     ssl <- SSL.connection ctx =<< mkSocket

     -- configure hostname used for certificate validation
     SSL.setTlsextHostName ssl hostname

     SSL.connect ssl

     return ssl


setupCaCertificates :: SSLContext -> Maybe FilePath -> IO ()
setupCaCertificates ctx mbPath =
  case mbPath of
    Nothing   -> contextLoadSystemCerts ctx
    Just path -> SSL.contextSetCAFile ctx path


setupCertificate :: SSLContext -> FilePath -> IO ()
setupCertificate ctx path
  =   SSL.contextSetCertificate ctx
  =<< PEM.readX509 -- EX
  =<< readFile path


setupPrivateKey :: SSLContext -> FilePath -> IO ()
setupPrivateKey ctx path =
  do str <- readFile path -- EX
     key <- PEM.readPrivateKey str PEM.PwNone -- TODO: add password support
     SSL.contextSetPrivateKey ctx key


verificationMode :: Bool {- ^ insecure -} -> SSL.VerificationMode
verificationMode insecure
  | insecure  = SSL.VerifyNone
  | otherwise = SSL.VerifyPeer
                  { SSL.vpFailIfNoPeerCert = True
                  , SSL.vpClientOnce       = True
                  , SSL.vpCallback         = Nothing
                  }
