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

  -- * Connections
  Connection,
  connect,
  recvLine,
  send,
  close,

  -- * Errors
  ConnectionFailure(..),
  ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Foldable
import           Data.List (intercalate)
import           Network (PortID(..))
import           Network.Socket (Socket, AddrInfo, PortNumber, HostName, Family)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketB
import           Network.Socks5
import           OpenSSL.Session (SSL, SSLContext)
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509 as SSL
import           OpenSSL.X509.SystemStore
import qualified OpenSSL.PEM as PEM

import           Hookup.OpenSSL (installVerification)


-- | Parameters for 'connect'.
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
  , tpInsecure           :: Bool -- ^ Disables certificate checking
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
  deriving Show

-- | 'displayException' implemented for prettier messages
instance Exception ConnectionFailure where
  displayException LineTruncated = "connection closed while reading line"
  displayException LineTooLong   = "line length exceeded maximum"
  displayException (ConnectionFailure xs) =
    "connection attempts failed due to: " ++
      intercalate ", " (map displayException xs)
  displayException (HostnameResolutionFailure x) =
    "hostname resolution failed: " ++ displayException x

------------------------------------------------------------------------
-- Opening sockets
------------------------------------------------------------------------

-- | Open a socket using the given parameters either directly or
-- via a SOCKS server.
openSocket :: ConnectionParams -> IO Socket
openSocket params =
  case cpSocks params of
    Nothing -> openSocket' (cpFamily params) (cpHost params) (cpPort params)
    Just sp -> openSocks sp (cpHost params) (cpPort params)


openSocks :: SocksParams -> HostName -> PortNumber -> IO Socket
openSocks sp h p =
  do socksConnectTo'
       (spHost sp) (PortNumber (spPort sp))
       h           (PortNumber p)


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


attemptConnections :: [IOError] -> [Socket.AddrInfo] -> IO Socket
attemptConnections exs [] = throwIO (ConnectionFailure exs)
attemptConnections exs (ai:ais) =
  do s <- socket' ai
     res <- try (Socket.connect s (Socket.addrAddress ai))
     case res of
       Left ex -> do Socket.close s
                     attemptConnections (ex:exs) ais
       Right{} -> return s


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


openNetworkHandle :: ConnectionParams -> IO NetworkHandle
openNetworkHandle params =
  do s <- openSocket params
     case cpTls params of
       Nothing -> return (Socket s)
       Just tp -> SSL <$> startTls (cpHost params) tp s


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
-- Throws 'IOError', 'SocksError', 'SSL.ProtocolError', 'ConnectionFailure'
connect :: ConnectionParams -> IO Connection
connect params =
  do h <- openNetworkHandle params
     b <- newMVar B.empty
     return (Connection b h)


-- | Close network connection.
close :: Connection -> IO ()
close (Connection _ h) = closeNetworkHandle h


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
recvLine :: Connection -> Int -> IO (Maybe ByteString)
recvLine (Connection buf h) n =
  modifyMVar buf $ \bs ->
    go (B.length bs) bs []
  where
    go bsn bs bss =
      case B.elemIndex 10 bs of
        Just i -> return (B.tail b,
                          Just (cleanEnd (B.concat (reverse (a:bss)))))
          where
            (a,b) = B.splitAt i bs
        Nothing ->
          do when (bsn >= n) (throwIO LineTooLong)
             more <- networkRecv h n
             if B.null more
               then if B.null bs then return (B.empty, Nothing)
                                 else throwIO LineTruncated
               else go (bsn + B.length more) more (bs:bss)


-- | Remove the trailing @'\\r'@ if one is found.
cleanEnd :: ByteString -> ByteString
cleanEnd bs
  | B.null bs || B.last bs /= 13 = bs
  | otherwise                    = B.init bs


-- | Send bytes on the network connection.
--
-- Throws: 'IOError', 'ProtocolError'
send :: Connection -> ByteString -> IO ()
send (Connection _ h) = networkSend h


------------------------------------------------------------------------


-- | Initiate a TLS session on the given socket destined for
-- the given hostname. When successful an active TLS connection
-- is returned with certificate verification successful when
-- requested.
startTls ::
  HostName  {- ^ server hostname  -} ->
  TlsParams {- ^ parameters       -} ->
  Socket    {- ^ connected socket -} ->
  IO SSL    {- ^ connected TLS    -}
startTls host tp s = SSL.withOpenSSL $
  do ctx <- SSL.context

     -- configure context
     SSL.contextSetCiphers          ctx (tpCipherSuite tp)
     installVerification            ctx host
     SSL.contextSetVerificationMode ctx (verificationMode (tpInsecure tp))
     SSL.contextAddOption           ctx SSL.SSL_OP_ALL
     SSL.contextRemoveOption        ctx SSL.SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS

     -- configure certificates
     setupCaCertificates ctx          (tpServerCertificate tp)
     traverse_ (setupCertificate ctx) (tpClientCertificate tp)
     traverse_ (setupPrivateKey  ctx) (tpClientPrivateKey  tp)

     -- add socket to context
     ssl <- SSL.connection ctx s
     SSL.setTlsextHostName ssl host
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
     key <- PEM.readPrivateKey str PEM.PwNone -- add password support
     SSL.contextSetPrivateKey ctx key


verificationMode :: Bool {- ^ insecure -} -> SSL.VerificationMode
verificationMode insecure
  | insecure  = SSL.VerifyNone
  | otherwise = SSL.VerifyPeer
                  { SSL.vpFailIfNoPeerCert = True
                  , SSL.vpClientOnce       = True
                  , SSL.vpCallback         = Nothing
                  }
