{-# language OverloadedStrings, TemplateHaskell #-}
module DCC where

import           Prelude        hiding (getContents, log)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State as S
import           Control.Monad.Trans.Except
import           Control.Monad         (unless)
import           Control.Exception     (bracket)
import           Control.Lens
import           Data.Functor          (void)
import           Data.Bits      hiding (complement)
import           Data.Word
import           Data.Function         (fix)
import           Network.BSD
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified System.IO                 as IO
import qualified Network.Socket.ByteString as B
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as B8

-- | ad-hoc structure for not confuse the args, only the name and size
-- are processed the C functions underlying expect Bytestrings and not
-- value as input
data DCCOffer = DCCOffer
     { _doName :: FilePath
     , _doAddr :: B.ByteString
     , _doPort :: B.ByteString
     , _doSize :: Int
     } deriving (Show)

data DCCError = ParseIPPort
              | ParseDottedIP
              | FailGetAdrr -- ^ inet_ntoa
              | NotFullRecv Int -- ^ Bytes that are missing
  deriving (Eq, Show)

type DottedIP = String
type IPPort   = String

-- Smart constructor
parseDccOffer :: FilePath -> [B.ByteString] -> DCCOffer
parseDccOffer outDir (bName : bAddr : bPort : bSize : _) =
    let fullpath = outDir ++ "/" ++ (B8.unpack bName)
        size     = read (B8.unpack bSize)
     in DCCOffer fullpath bAddr bPort size

-- Binary utilities

-- todo(slack): Should I consider non x86 platforms? importing Binary
-- would offload that decision.
-- | Given a Word32 ie 4 chained bytes (a,b,c,d) return the reverse of
-- such chain (d,c,b,a). Useful for the network byte order IP on the CTCP
-- DCC message
complement :: HostAddress -> HostAddress
complement h =
  let (a,b,c,d) = (h .&. 0xFF000000, h .&. 0xFF0000, h .&. 0xFF00, h .&. 0xFF)
   in (rotateR d 8) + (rotateL c 8) + (rotateR b 8) + (rotateL a 8)

-- | given a number forms a bytestring with each digit on a separated
-- Word8 in network byte-order
int2BS :: Int -> B.ByteString
int2BS i | w <- (fromIntegral i :: Word32) =
    B.pack [ (fromIntegral (shiftR w 24) :: Word8)
           , (fromIntegral (shiftR w 16) :: Word8)
           , (fromIntegral (shiftR w  8) :: Word8)
           , (fromIntegral w             :: Word8)]

-- Connection processing

-- | Utility function for parsing Port, file size and HostAddress. For this
-- last one we need Num because we rely on its instance for construction.
parseBS :: Num a => B.ByteString -> Maybe a
parseBS = fmap (fromInteger . fst) . B8.readInteger

parseDccIP :: DCCOffer -> ExceptT DCCError IO (DottedIP, IPPort)
parseDccIP (DCCOffer _ bAddr bPort _)
  | Just addr   <- parseBS bAddr =
        lift $ do dottedIP <- inet_ntoa (complement addr)
                  return (dottedIP, B8.unpack bPort)
  | otherwise = throwE ParseIPPort

newSocket :: AddrInfo -> IO Socket
newSocket addr = do
     sock <- socket AF_INET Stream defaultProtocol
     connect sock (addrAddress addr)
     return sock

partnerInfo :: (DottedIP, IPPort) -> ExceptT DCCError IO AddrInfo
partnerInfo (dottedIP, ipPort) =
  let flags = [AI_NUMERICHOST, AI_NUMERICSERV]
      hints = defaultHints { addrFlags = flags }
   in lift (getAddrInfo (Just hints) (Just dottedIP) (Just ipPort))
      >>= maybe (throwE FailGetAdrr) return . preview folded

getPackets :: FilePath -- ^ Name media
           -> Int      -- ^ File size
           -> AddrInfo -> ExceptT DCCError IO ()
getPackets name totalSize addr =
  do receivedSize <- lift $ bracket acquire release receive
     let delta = (totalSize - receivedSize)
     if delta > 0 then throwE (NotFullRecv delta) else return ()
  where
    bufferSize = 4096

    acquire :: IO (IO.Handle,Socket)
    acquire = (,) <$> (IO.openFile name IO.WriteMode)
                  <*> newSocket addr

    release :: (IO.Handle,Socket) -> IO ()
    release (hdl, sock) = IO.hClose hdl >> close sock

    receive :: (IO.Handle,Socket) -> IO Int
    receive (hdl, sock) =
        flip execStateT 0 . fix $ \loop -> do
            mediaData <- lift (B.recv sock bufferSize)
            unless (B.null mediaData) $ do
                S.modify' (+ (B.length mediaData))
                currentSize <- S.get
                lift $ B.hPut hdl mediaData
                       >> B.send sock (int2BS currentSize)
                loop

-- Entry point of connection processing
-- todo(slack). Do something on DCCError
dcc_recv :: DCCOffer -> IO ()
dcc_recv offer@(DCCOffer name _ _ size) =
   void . runExceptT $
       parseDccIP offer >>= partnerInfo >>= getPackets name size
