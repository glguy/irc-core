{-|
Module      : Hookup
Description : Network connections generalized over TLS and SOCKS
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a uniform interface to network connections
with optional support for TLS and SOCKS.

This library is careful to support both IPv4 and IPv6. It will attempt to
all of the addresses that a domain name resolves to until one the first
successful connection.

Use 'connect' and 'close' to establish and close network connections.

Use 'recv', 'recvLine', and 'send' to receive and transmit data on an
open network connection.

TLS and SOCKS parameters can be provided. When both are provided a connection
will first be established to the SOCKS server and then the TLS connection will
be established through that proxy server. This is most useful when connecting
through a dynamic port forward of an SSH client via the @-D@ flag.

-}
module Hookup
  (
  -- * Connections
  Connection,
  connect,
  connectWithSocket,
  close,

  -- * Reading and writing data
  recv,
  recvLine,
  send,
  putBuf,

  -- * Configuration
  ConnectionParams(..),
  SocksParams(..),
  TlsParams(..),
  PEM.PemPasswordSupply(..),
  defaultFamily,
  defaultTlsParams,


  -- * Errors
  ConnectionFailure(..),
  CommandReply(..)

  -- * SSL Information
  , getClientCertificate
  , getPeerCertificate
  , getPeerCertFingerprintSha1
  , getPeerCertFingerprintSha256
  , getPeerCertFingerprintSha512
  , getPeerPubkeyFingerprintSha1
  , getPeerPubkeyFingerprintSha256
  , getPeerPubkeyFingerprintSha512
  ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           System.IO.Error (isDoesNotExistError, ioeGetErrorString)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Foldable
import           Data.List (intercalate, partition)
import           Network.Socket (Socket, AddrInfo, PortNumber, HostName, Family)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketB
import           OpenSSL.Session (SSL, SSLContext)
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSL
import           OpenSSL.X509.SystemStore
import           OpenSSL.X509 (X509)
import qualified OpenSSL.X509 as X509
import qualified OpenSSL.PEM as PEM
import qualified OpenSSL.EVP.Digest as Digest
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as Parser

import           Hookup.OpenSSL (installVerification, getPubKeyDer)
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
-- using a SOCKS (version 5) proxy.
--
-- When a 'TlsParams' is provided the connection negotiate TLS at connect
-- time in order to protect the stream.
data ConnectionParams = ConnectionParams
  { cpFamily :: Family           -- ^ IP Protocol family (default 'AF_UNSPEC')
  , cpHost  :: HostName          -- ^ Destination host
  , cpPort  :: PortNumber        -- ^ Destination TCP port
  , cpSocks :: Maybe SocksParams -- ^ Optional SOCKS parameters
  , cpTls   :: Maybe TlsParams   -- ^ Optional TLS parameters
  }


-- | SOCKS connection parameters
data SocksParams = SocksParams
  { spHost :: HostName   -- ^ SOCKS server host
  , spPort :: PortNumber -- ^ SOCKS server port
  }


-- | TLS connection parameters. These parameters are passed to
-- OpenSSL when making a secure connection.
data TlsParams = TlsParams
  { tpClientCertificate  :: Maybe FilePath -- ^ Path to client certificate
  , tpClientPrivateKey   :: Maybe FilePath -- ^ Path to client private key
  , tpClientPrivateKeyPassword :: PEM.PemPasswordSupply -- ^ Private key decryption password
  , tpServerCertificate  :: Maybe FilePath -- ^ Path to CA certificate bundle
  , tpCipherSuite        :: String -- ^ OpenSSL cipher suite name (e.g. @\"HIGH\"@)
  , tpInsecure           :: Bool -- ^ Disables certificate checking when 'True'
  }

-- | Type for errors that can be thrown by this package.
data ConnectionFailure
  -- | Failure during 'getAddrInfo' resolving remote host
  = HostnameResolutionFailure HostName String
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
  displayException (HostnameResolutionFailure h s) =
    "hostname resolution failed (" ++ h ++ "): "  ++ s
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
-- system CA root, @\"HIGH\"@ cipher suite, and which validate hostnames.
defaultTlsParams :: TlsParams
defaultTlsParams = TlsParams
  { tpClientCertificate  = Nothing
  , tpClientPrivateKey   = Nothing
  , tpClientPrivateKeyPassword = PEM.PwNone
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
       Right ais -> attempt (interleaveAddressFamilies ais)
       Left  ioe
         | isDoesNotExistError ioe ->
             throwIO (HostnameResolutionFailure h (ioeGetErrorString ioe))
         | otherwise -> throwIO ioe -- unexpected

attempt ::
  [Socket.AddrInfo] {- ^ candidate AddrInfos -} ->
  IO Socket         {- ^ connected socket    -}
attempt ais =
  do comm    <- newEmptyMVar
     threads <- zipWithM (attemptThread comm) [0..] ais
     s       <- gather (length ais) [] comm

     -- kill the other attempts and don't bother waiting for that to finish
     forkIO (traverse_ killThread threads)

     pure s

attemptThread ::
  MVar (Either IOError Socket) ->
  Int {- 0-based attempt index -} ->
  AddrInfo ->
  IO ThreadId
attemptThread comm i ai =
  forkIO $
  do let connAttemptDelay = 150 * 1000 -- 150ms
     threadDelay (connAttemptDelay * i)
     putMVar comm =<< try (connectToAddrInfo ai)

-- Either gather all of the errors possible and throw an exception or
-- return the first successful socket.
gather ::
  Int {- ^ potential errors remaining -} ->
  [IOError] {- ^ errors gathered so far -} ->
  MVar (Either IOError Socket) ->
  IO Socket
gather 0 exs _ = throwIO (ConnectionFailure exs)
gather n exs comm =
  do res <- takeMVar comm
     case res of
       Right s -> pure s
       Left ex -> gather (n-1) (ex:exs) comm

-- | Alternate list of addresses between IPv6 and other (IPv4) addresses.
interleaveAddressFamilies :: [AddrInfo] -> [AddrInfo]
interleaveAddressFamilies ais = interleave sixes others
  where
    (sixes, others) = partition is6 ais
    is6 ai = Socket.AF_INET6 == Socket.addrFamily ai

    interleave (x:xs) (y:ys) = x : y : interleave xs ys
    interleave []     ys     = ys
    interleave xs     []     = xs

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

data NetworkHandle = SSL (Maybe X509) SSL | Socket Socket


openNetworkHandle ::
  ConnectionParams {- ^ parameters             -} ->
  IO Socket        {- ^ socket creation action -} ->
  IO NetworkHandle {- ^ open network handle    -}
openNetworkHandle params mkSocket =
  case cpTls params of
    Nothing  -> Socket <$> mkSocket
    Just tls ->
        do (clientCert, ssl) <- startTls tls (cpHost params) mkSocket
           pure (SSL clientCert ssl)


closeNetworkHandle :: NetworkHandle -> IO ()
closeNetworkHandle (Socket s) = Socket.close s
closeNetworkHandle (SSL _ s) =
  do SSL.shutdown s SSL.Unidirectional
     traverse_ Socket.close (SSL.sslSocket s)

networkSend :: NetworkHandle -> ByteString -> IO ()
networkSend (Socket s) = SocketB.sendAll s
networkSend (SSL  _ s) = SSL.write       s

networkRecv :: NetworkHandle -> Int -> IO ByteString
networkRecv (Socket s) = SocketB.recv s
networkRecv (SSL  _ s) = SSL.read     s


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
--
-- Throws: 'IOError', 'SSL.ConnectionAbruptlyTerminated', 'SSL.ProtocolError'
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
-- Throws: 'SSL.ConnectionAbruptlyTerminated', 'SSL.ProtocolError', 'ConnectionFailure', 'IOError'
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
-- Throws: 'IOError', 'SSL.ProtocolError'
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
  IO (Maybe X509, SSL) {- ^ (client certificate, connected TLS) -}
startTls tp hostname mkSocket = SSL.withOpenSSL $
  do ctx <- SSL.context

     -- configure context
     SSL.contextSetCiphers          ctx (tpCipherSuite tp)
     installVerification            ctx hostname
     SSL.contextSetVerificationMode ctx (verificationMode (tpInsecure tp))
     SSL.contextAddOption           ctx SSL.SSL_OP_ALL
     SSL.contextRemoveOption        ctx SSL.SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS

     -- configure certificates
     setupCaCertificates ctx (tpServerCertificate tp)
     clientCert <- traverse (setupCertificate ctx) (tpClientCertificate tp)
     traverse_ (setupPrivateKey ctx (tpClientPrivateKeyPassword tp)) (tpClientPrivateKey tp)

     -- add socket to context
     -- creation of the socket is delayed until this point to avoid
     -- leaking the file descriptor in the cases of exceptions above.
     ssl <- SSL.connection ctx =<< mkSocket

     -- configure hostname used for certificate validation
     SSL.setTlsextHostName ssl hostname

     SSL.connect ssl

     return (clientCert, ssl)


setupCaCertificates :: SSLContext -> Maybe FilePath -> IO ()
setupCaCertificates ctx mbPath =
  case mbPath of
    Nothing   -> contextLoadSystemCerts ctx
    Just path -> SSL.contextSetCAFile ctx path


setupCertificate :: SSLContext -> FilePath -> IO X509
setupCertificate ctx path =
  do x509 <- PEM.readX509 =<< readFile path -- EX
     SSL.contextSetCertificate ctx x509
     pure x509


setupPrivateKey :: SSLContext -> PEM.PemPasswordSupply -> FilePath -> IO ()
setupPrivateKey ctx password path =
  do str <- readFile path -- EX
     key <- PEM.readPrivateKey str password
     SSL.contextSetPrivateKey ctx key


verificationMode :: Bool {- ^ insecure -} -> SSL.VerificationMode
verificationMode insecure
  | insecure  = SSL.VerifyNone
  | otherwise = SSL.VerifyPeer
                  { SSL.vpFailIfNoPeerCert = True
                  , SSL.vpClientOnce       = True
                  , SSL.vpCallback         = Nothing
                  }

-- | Get peer certificate if one exists.
getPeerCertificate :: Connection -> IO (Maybe X509.X509)
getPeerCertificate (Connection _ h) =
  case h of
    Socket{} -> return Nothing
    SSL _ ssl -> SSL.getPeerCertificate ssl

-- | Get peer certificate if one exists.
getClientCertificate :: Connection -> Maybe X509.X509
getClientCertificate (Connection _ h) =
  case h of
    Socket{} -> Nothing
    SSL c _  -> c

getPeerCertFingerprintSha1 :: Connection -> IO (Maybe ByteString)
getPeerCertFingerprintSha1 = getPeerCertFingerprint "sha1"

getPeerCertFingerprintSha256 :: Connection -> IO (Maybe ByteString)
getPeerCertFingerprintSha256 = getPeerCertFingerprint "sha256"

getPeerCertFingerprintSha512 :: Connection -> IO (Maybe ByteString)
getPeerCertFingerprintSha512 = getPeerCertFingerprint "sha512"

getPeerCertFingerprint :: String -> Connection -> IO (Maybe ByteString)
getPeerCertFingerprint name h =
   do mb <- getPeerCertificate h
      case mb of
        Nothing -> return Nothing
        Just x509 ->
          do der <- X509.writeDerX509 x509
             mbdigest <- Digest.getDigestByName name
             case mbdigest of
               Nothing -> return Nothing
               Just digest -> return $! Just $! Digest.digestLBS digest der

getPeerPubkeyFingerprintSha1 :: Connection -> IO (Maybe ByteString)
getPeerPubkeyFingerprintSha1 = getPeerPubkeyFingerprint "sha1"

getPeerPubkeyFingerprintSha256 :: Connection -> IO (Maybe ByteString)
getPeerPubkeyFingerprintSha256 = getPeerPubkeyFingerprint "sha256"

getPeerPubkeyFingerprintSha512 :: Connection -> IO (Maybe ByteString)
getPeerPubkeyFingerprintSha512 = getPeerPubkeyFingerprint "sha512"


getPeerPubkeyFingerprint :: String -> Connection -> IO (Maybe ByteString)
getPeerPubkeyFingerprint name h =
   do mb <- getPeerCertificate h
      case mb of
        Nothing -> return Nothing
        Just x509 ->
          do der <- getPubKeyDer x509
             mbdigest <- Digest.getDigestByName name
             case mbdigest of
               Nothing -> return Nothing
               Just digest -> return $! Just $! Digest.digestBS digest der
