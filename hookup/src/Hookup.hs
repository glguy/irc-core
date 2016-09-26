{-# Language OverloadedStrings #-}
module Hookup where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Concurrent
import Network.Socks5
import Network.Socket (Socket, AddrInfo, PortNumber, HostName)
import Network (PortID(..))
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketB
import OpenSSL.Session (SSL)
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509 as SSL
import OpenSSL.X509.SystemStore
import Control.Monad

data ConnectionParams = ConnectionParams
  { cpHost  :: HostName
  , cpPort  :: PortNumber
  , cpSocks :: Maybe SocksParams
  , cpTls   :: Maybe TlsParams
  }

data SocksParams = SocksParams
  { spHost :: HostName
  , spPort :: PortNumber
  }

data TlsParams = TlsParams
  { tpClientCertificate :: Maybe FilePath
  , tpServerCertificates :: [FilePath]
  , tpInsecure :: Bool
  }


data ConnectionFailure
  = HostnameResolutionFailure IOError
  | ConnectionFailure [IOError]
  deriving Show
instance Exception ConnectionFailure


withHookupDo :: IO a -> IO a
withHookupDo = SSL.withOpenSSL


openSocket :: ConnectionParams -> IO Socket
openSocket params =
  case cpSocks params of
    Nothing -> openSocket'  (cpHost params) (cpPort params)
    Just sp -> openSocks sp (cpHost params) (cpPort params)


openSocks :: SocksParams -> HostName -> PortNumber -> IO Socket
openSocks sp h p =
  socksConnectTo' -- EX
    (spHost sp) (PortNumber (spPort sp))
    h           (PortNumber p)


openSocket' :: HostName -> PortNumber -> IO Socket
openSocket' h p =
  do let hints = Socket.defaultHints
           { Socket.addrSocketType = Socket.Stream
           , Socket.addrFlags      = [Socket.AI_ADDRCONFIG]
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


data Connection = Connection (MVar ByteString) NetworkHandle

connect :: ConnectionParams -> IO Connection
connect params =
  do h <- openNetworkHandle params
     b <- newMVar B.empty
     return (Connection b h)


close :: Connection -> IO ()
close (Connection _ h) = closeNetworkHandle h


recvLine :: Connection -> Int -> IO ByteString
recvLine (Connection buf h) n = modifyMVar buf $ \bs -> go (B.length bs) bs []
  where
    go bsn bs bss =
      case B.elemIndex 10 bs of
        Just i -> return (B.tail b, B.concat (reverse (a:bss)))
          where
            (a,b) = B.splitAt i bs
        Nothing
          | bsn >= n -> fail ("Line too long: " ++ show (bsn,n,bs,bss))
          | otherwise ->
              do more <- networkRecv h n
                 go (bsn + B.length more) more (bs:bss)


send :: Connection -> ByteString -> IO ()
send (Connection _ h) = networkSend h


------------------------------------------------------------------------


startTls :: HostName -> TlsParams -> Socket -> IO SSL
startTls host tp s =
  do cxt <- SSL.context
     contextLoadSystemCerts cxt
     SSL.contextSetCiphers cxt "HIGH"
     SSL.installVerification cxt host
     SSL.contextSetVerificationMode cxt (verificationMode (tpInsecure tp))
     SSL.contextAddOption cxt SSL.SSL_OP_ALL
     ssl <- SSL.connection cxt s
     SSL.connect ssl

     unless (tpInsecure tp) $
       do verified <- SSL.getVerifyResult ssl
          unless verified $
            fail "verification failed"

     return ssl


verificationMode :: Bool {- ^ insecure -} -> SSL.VerificationMode
verificationMode True  = SSL.VerifyNone
verificationMode False = SSL.VerifyPeer
  { SSL.vpFailIfNoPeerCert = True
  , SSL.vpClientOnce       = True
  , SSL.vpCallback         = Nothing
  }


------------------------------------------------------------------------

{-
main = SSL.withOpenSSL $
  do c <- connect ConnectionParams
            { cpHost = "glguy.net"
            , cpPort = 7000
            , cpSocks = Nothing
            , cpTls = Just TlsParams
                { tpClientCertificate = Nothing
                , tpServerCertificates = []
                , tpInsecure = False
                }
            }
     print "a"
     send c "PASS\r\n"
     send c "NICK glguy\r\n"
     send c "USER glguy 8 * glguy\r\n"
     print "b"
     print =<< recv c 1024
     print "c"
     close c
-}
