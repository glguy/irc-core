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
  -- * Library initialization
  withHookupDo,

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
import           Network (PortID(..))
import           Network.Socket (Socket, AddrInfo, PortNumber, HostName)
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
  { cpHost  :: HostName          -- ^ Destination host
  , cpPort  :: PortNumber        -- ^ Destination TCP port
  , cpSocks :: Maybe SocksParams -- ^ Optional SOCKS5 parameters
  , cpTls   :: Maybe TlsParams   -- ^ Optional TLS parameters
  }


-- | SOCKS5 connection parameters
data SocksParams = SocksParams
  { spHost :: HostName   -- ^ SOCKS server host
  , spPort :: PortNumber -- ^ SOCKS server port
  }


-- | TLS connection parameters
data TlsParams = TlsParams
  { tpClientCertificate  :: Maybe FilePath
  , tpClientPrivateKey   :: Maybe FilePath
  , tpServerCertificate  :: Maybe FilePath
  , tpCipherSuite        :: String
  , tpInsecure           :: Bool
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

instance Exception ConnectionFailure


-- | Perform an action within an initialized context. This initializes
-- the OpenSSL library.
withHookupDo :: IO a -> IO a
withHookupDo = SSL.withOpenSSL

------------------------------------------------------------------------
-- Opening sockets
------------------------------------------------------------------------

-- | Open a socket using the given parameters either directly or
-- via a SOCKS server.
openSocket :: ConnectionParams -> IO Socket
openSocket params =
  case cpSocks params of
    Nothing -> openSocket'  (cpHost params) (cpPort params)
    Just sp -> openSocks sp (cpHost params) (cpPort params)


openSocks :: SocksParams -> HostName -> PortNumber -> IO Socket
openSocks sp h p =
  do socksConnectTo'
       (spHost sp) (PortNumber (spPort sp))
       h           (PortNumber p)


openSocket' :: HostName -> PortNumber -> IO Socket
openSocket' h p =
  do let hints = Socket.defaultHints
           { Socket.addrSocketType = Socket.Stream
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
       Left ex -> attemptConnections (ex:exs) ais
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
closeNetworkHandle (SSL s) = SSL.shutdown s SSL.Unidirectional
closeNetworkHandle (Socket s) = Socket.close s

networkSend :: NetworkHandle -> ByteString -> IO ()
networkSend (Socket s) = SocketB.sendAll s
networkSend (SSL    s) = SSL.write       s

networkRecv :: NetworkHandle -> Int -> IO ByteString
networkRecv (Socket s) = SocketB.recv s
networkRecv (SSL    s) = SSL.read     s


------------------------------------------------------------------------
-- Sockets with a receive buffer
------------------------------------------------------------------------

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
-- @"\r\n"@ and @"\n"@ are recognized.
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


-- | Remove the trailing @'\r'@ if one is found.
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
-- the given hostname.
startTls ::
  HostName  {- ^ server hostname -} ->
  TlsParams {- ^ parameters      -} ->
  Socket    {- ^ open socket     -} ->
  IO SSL
startTls host tp s =
  do cxt <- SSL.context

     -- configure context
     SSL.contextSetCiphers          cxt (tpCipherSuite tp)
     installVerification            cxt host
     SSL.contextSetVerificationMode cxt (verificationMode (tpInsecure tp))
     SSL.contextAddOption           cxt SSL.SSL_OP_ALL

     -- configure certificates
     setupCaCertificates cxt          (tpServerCertificate tp)
     traverse_ (setupCertificate cxt) (tpClientCertificate tp)
     traverse_ (setupPrivateKey  cxt) (tpClientPrivateKey  tp)

     -- add socket to context
     ssl <- SSL.connection cxt s

     SSL.connect ssl

     return ssl


setupCaCertificates :: SSLContext -> Maybe FilePath -> IO ()
setupCaCertificates cxt mbPath =
  case mbPath of
    Nothing   -> contextLoadSystemCerts cxt
    Just path -> SSL.contextSetCAFile cxt path


setupCertificate :: SSLContext -> FilePath -> IO ()
setupCertificate cxt path
  =   SSL.contextSetCertificate cxt
  =<< PEM.readX509 -- EX
  =<< readFile path


setupPrivateKey :: SSLContext -> FilePath -> IO ()
setupPrivateKey cxt path =
  do str <- readFile path -- EX
     key <- PEM.readPrivateKey str PEM.PwNone -- add password support
     SSL.contextSetPrivateKey cxt key


verificationMode :: Bool {- ^ insecure -} -> SSL.VerificationMode
verificationMode insecure
  | insecure  = SSL.VerifyNone
  | otherwise = SSL.VerifyPeer
                  { SSL.vpFailIfNoPeerCert = True
                  , SSL.vpClientOnce       = True
                  , SSL.vpCallback         = Nothing
                  }
