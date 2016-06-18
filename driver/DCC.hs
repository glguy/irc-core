{-# language OverloadedStrings #-}
module DCC where

import           Prelude        hiding (getContents, log)
import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State as S
import           Control.Monad.Trans.Except
import           Control.Exception     (bracket)
import           Control.Lens
import           Data.Bits      hiding (complement)
import           Data.Function         (fix)
import           Data.Int              (Int64)
import           Network.BSD
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket            as SS
import qualified System.IO                 as IO
import qualified Network.Socket.ByteString as B
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as B8

-- | ad-hoc structure for not confuse the args
data DCCOffer = DCCOffer
     { doName :: B.ByteString
     , doAddr :: B.ByteString
     , doPort :: B.ByteString
     , doSize :: B.ByteString
     }

data DCCError = ParseIPPort
              | ParseDottedIP
              | FailGetAdrr -- ^ inet_ntoa
              | NotFullRecv Int -- ^ Bytes that are missing
  deriving (Eq, Show)

type DottedIP = String
type IPPort   = String

-- Binary utilities

-- | Given a Word32 ie 4 chained bytes (a,b,c,d) return the reverse of
-- such chain (d,c,b,a). Useful for the network byte order IP on the CTCP
-- DCC message
complement :: HostAddress -> HostAddress
complement h =
  let (a,b,c,d) = (h .&. 0xFF000000, h .&. 0xFF0000, h .&. 0xFF00, h .&. 0xFF)
   in (rotateR d 8) + (rotateL c 8) + (rotateR b 8) + (rotateL a 8)

-- | Utility function for parsing Port, file size and HostAddress. For this
-- last one we need Num because we rely on its instance for construction.
parseBS :: (Num a) => B.ByteString -> Maybe a
parseBS = fmap (fromInteger . fst) . B8.readInteger

parseDccIP :: DCCOffer -> ExceptT DCCError IO (DottedIP, IPPort)
parseDccIP (DCCOffer _ bAddr bPort _)
  | Just addr   <- parseBS bAddr =
        lift $ do dottedIP <- inet_ntoa (complement addr)
                  return (dottedIP, B8.unpack bPort)
  | otherwise = throwE ParseIPPort

-- todo slack
newFileHandle :: String -> IO IO.Handle
newFileHandle name = IO.openFile ("~/" ++ name) IO.WriteMode

newSocket :: AddrInfo -> IO Socket
newSocket addr = do
     sock <- socket AF_INET Stream defaultProtocol
     connect sock (addrAddress addr)
     return sock

-- | given a number forms a bytestring with each digit on a separated Word8
int2BS :: Word -> B.ByteString
int2BS n = let go b = rotateR n (b * 8) .&. 0xFF
            in B.pack . map go $ [0,1,2,3]

partnerInfo :: (DottedIP, IPPort) -> ExceptT DCCError IO AddrInfo
partnerInfo (dottedIP, ipPort) =
  let flags = [AI_NUMERICHOST, AI_NUMERICSERV]
      hints = defaultHints { addrFlags = flags }
   in lift (getAddrInfo (Just hints) (Just dottedIP) (Just ipPort))
      >>= maybe (throwE FailGetAdrr) return . preview folded

getPackets :: String   -- ^ Name media
           -> Int      -- ^ File size
           -> AddrInfo
           -> ExceptT DCCError IO ()
getPackets name totalSize addr =
  do receivedSize <- lift $ bracket (acquire) (uncurry release)
                                    (uncurry receive)
     let delta = (totalSize - receivedSize)
     if delta > 0 then throwE (NotFullRecv delta) else return ()
  where
    bufferSize = 4096

    acquire :: IO (IO.Handle, Socket)
    acquire = (,) <$> newFileHandle name <*> newSocket addr

    release :: IO.Handle -> Socket -> IO ()
    release hdl sock = IO.hClose hdl >> close sock)

    receive :: IO.Handle -> Socket -> IO Int
    receive hdl sock =
        flip execState 0 . fix $ \loop -> do
            mediaData <- lift (B.recv sock bufferSize)
            if (B.null mediaData)
              then return () -- we only care about the state
              else do S.modify' (+ (B.length mediaData))
                      currentSize <- S.get
                      lift $ B.hPut hdl mediaData
                             >> B.send sock (int2BS currentSize)
                      loop

dcc_recv :: DCCOffer -> IO ()
dcc_recv offer@(DCCOffer bName _ _ bSize) =
  let size = read (B8.unpack bSize)
      name = B8.unpack bName
   in void . runExceptT $
          parseDccIP offer
          >>= partnerInfo
          >>= liftIO . getPackets name size
