{-# Language TemplateHaskell, OverloadedStrings, BangPatterns
    , ScopedTypeVariables, TypeApplications #-}

{-|
Module      : Client.State.DCC
Description : CTCP DCC transfer handling
Copyright   : (c) Ruben Astudillo, 2019
License     : ISC
Maintainer  : ruben.astud@gmail.com

This module provides ADTs and functions to deal with DCC SEND/ACCEPT
request and how start such transfers.
-}

module Client.State.DCC
  (
  -- * DCC offers
    DCCOffer(..)
  , dccNetwork
  , dccFromInfo
  , dccFromIP
  , dccPort
  , dccFileName
  , dccSize
  , dccOffset
  -- * DCC transfer
  , DCCTransfer(..)
  , dtThread
  , dtProgress
  -- * DCC Update
  , DCCUpdate(..)
  -- * Transfer a DCCOffer
  , supervisedDownload
  -- * Parser for DCC request
  , parseSEND
  , parseACCEPT
  -- * Miscellaneous
  , getFileOffset
  ) where

import           Control.Applicative (Alternative(..))
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Exception as E
import           Control.Exception ( bracket, IOException, AsyncException)
import           Control.Lens hiding (from)
import           Control.Monad (unless)
import           Control.Monad (when)
import           Data.Attoparsec.Text
import qualified Data.ByteString as B
import           Data.ByteString.Builder (word32BE, toLazyByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.IntMap (Key)
import           Data.IntMap as I hiding (size)
import           Data.List (find)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word32, Word64)
import           Hookup
import           Irc.UserInfo (UserInfo(..))
import           Network.Socket ( HostName, PortNumber, Family(..)
                                , hostAddressToTuple )
import           System.IO (withFile, IOMode(..), openFile, hClose, hFileSize)
import           System.Directory (doesFileExist)
import           System.Environment (getEnv)
import           System.FilePath ((</>))

-- | All the neccesary information to start the download
data DCCOffer = DCCOffer
  { _dccNetwork  :: Text
  , _dccFromInfo :: UserInfo
  , _dccFromIP   :: HostName -- ^ String of the ipv4 representation
  , _dccPort     :: PortNumber
  , _dccFileName :: Text
  , _dccSize     :: Word32 -- ^ Size of the whole file, per protocol
                           --   restricted to 32-bits
  , _dccOffset   :: Word32 -- ^ Byte from where the transmission starts
  } deriving (Show, Eq)


makeLenses ''DCCOffer


data DCCTransfer = DCCTransfer
  { _dtThread      :: Maybe (Async ()) -- ^ If Nothing, the thread was killed
                                       --   and stopped.
  , _dtProgress    :: Word32 -- ^ Percentage of progress
  }

makeLenses ''DCCTransfer


data DCCUpdate
  = PercentUpdate Key Word32
  | Finished Key
  | InterruptedTransfer Key
  | Accept Key PortNumber Word32 -- update that DCC RESUME triggers
  deriving (Show, Eq)            -- Word32 is the offset


supervisedDownload :: Maybe FilePath -> Key -> TChan DCCUpdate
                   -> DCCOffer -> IO ()
supervisedDownload mdir key updateChan offer =
  withAsync (startDownload mdir key updateChan offer) $ \realTransferThread ->
    do upd <- E.catches (Finished key <$ wait realTransferThread)
                [ E.Handler (\(_ :: IOException) ->
                               return (InterruptedTransfer key))
                -- user killed, thrown by async >= 2.2
                , E.Handler (\(_ :: AsyncCancelled) ->
                               return (InterruptedTransfer key))
                -- Compatibility hack
                -- user killed, thrown by async <= 2.1.1 on cancel
                , E.Handler (\(_ :: AsyncException) ->
                               return (InterruptedTransfer key))
                ]
       atomically $ writeTChan updateChan upd

-- | Process the DCCOffer starting the exchange
startDownload :: Maybe FilePath -> Key -> TChan DCCUpdate
              -> DCCOffer -> IO ()
startDownload mdir key updateChan offer@(DCCOffer _ _ from port name totalSize offset) = do
  home <- getEnv "HOME"
  let openMode = if offset > 0 then AppendMode else WriteMode
      filepath = (maybe home id mdir) </> Text.unpack name
  bracket (connect param) close $ \conn ->
    bracket (openFile filepath openMode) hClose $ \hdl ->
      do -- Has to decouple @send@ from @recv@, tells how much
         -- have we downloaded.
         recvChan1 <- atomically newTChan
         recvChan2 <- atomically $ dupTChan recvChan1

         -- Two threads, one for @send@ the progress to the
         -- server and another to signal how much progress
         -- have we done to the main thread. `withAsync` guarrantee
         -- correct exception handling when the user cancels the
         -- transfer.
         withAsync (sendStream totalSize conn recvChan1) $ \outThread ->
           withAsync (report offer key recvChan2 updateChan)
             $ \_reportThread -> do recvSendLoop 0 recvChan1 conn hdl
                                    wait outThread
  where
    param = ConnectionParams
              { cpFamily = AF_INET, cpHost = from
              , cpPort = port, cpSocks = Nothing
              , cpTls = Nothing }

    buffSize = 4 * 1024 * 1024

    recvSendLoop size chan conn hdl =
      do bytes <- recv conn buffSize
         unless (B.null bytes) $
           do B.hPut hdl bytes
              let newSize = size + fromIntegral (B.length bytes)
              atomically $ writeTChan chan newSize
              recvSendLoop newSize chan conn hdl


-- | @send@ing the current size to the fileserver. As an
--   independent acknowledgement stream, it doesn't match the protocol,
--   but matches what other clients and servers do in practice.
sendStream :: Word32 -> Connection -> TChan Word32 -> IO ()
sendStream maxSize conn chan =
  do val <- atomically $ readTChan chan
     let valBE = toStrict . toLazyByteString $ word32BE val
     send conn valBE
     unless (val >= maxSize) (sendStream maxSize conn chan)


report :: DCCOffer -> Key -> TChan Word32 -> TChan DCCUpdate -> IO ()
report offer key input output = compareAndUpdate (percent offset totalsize)
  where
    offset = _dccOffset offer
    totalsize = _dccSize offer

    compareAndUpdate :: Word32 -> IO ()
    compareAndUpdate prevPercent =
      do curSize <- atomically $ readTChan input
         let -- curPercent :: Word64 so the (* 100) doesn't overflow.
             curPercent = percent (offset + curSize) totalsize
             updateEv   = PercentUpdate key curPercent
         if (curPercent == 100)
           then atomically (writeTChan output updateEv)
           else do when (curPercent > prevPercent)
                        $ atomically (writeTChan output updateEv)
                   compareAndUpdate curPercent

    percent :: Word32 -> Word32 -> Word32
    percent a total = fromIntegral $
      ((fromIntegral @_ @Word64 a) * 100) `div` (fromIntegral total)


parseSEND :: Text -> UserInfo -> Text -> Either String DCCOffer
parseSEND network userFrom text = parseOnly (sendFormat network userFrom) text

sendFormat :: Text -> UserInfo -> Parser DCCOffer
sendFormat network userFrom =
  let fun name addr port totalsize =
        DCCOffer network userFrom addr port name totalsize 0
  in fun <$> (string "SEND" *> space *> nameFormat)
             <*> (ipv4Dotted <$> (space *> decimal))
             <*> (space *> decimal)
             <*> (space *> decimal)

-- | DCC RESUME counterpart.
parseACCEPT :: IntMap DCCOffer -> UserInfo -> Text -> Maybe DCCUpdate
parseACCEPT offers userFrom text =
  let offerList = I.toDescList offers
      predicate (fileName, _, _) (_, offer)=
        (_dccFileName offer == fileName) && userFrom == (_dccFromInfo offer)
  in case parseOnly acceptFormat text of
       Left _ -> Nothing
       Right args@(_, port, offset) ->
         (\(key, _) -> Accept key port offset)
             <$> find (predicate args) offerList


acceptFormat :: Parser (Text, PortNumber, Word32)
acceptFormat =
  (,,) <$> (string "ACCEPT " *> nameFormat)
       <*> (space *> decimal) -- port
       <*> (space *> decimal) -- offset

nameFormat :: Parser Text
nameFormat = try quotedName <|> noSpaceName
  where
    quotedName = char '\"' *> takeWhile1 (\c -> c /= '\"') <* char '\"'
    noSpaceName = takeWhile1 (\c -> c /= ' ')

-- | Asumming little-endian
ipv4Dotted :: Word32 -> HostName
ipv4Dotted addr =
  let bigToLittleEndian (a, b, c, d) = (d, c, b, a)
      ipv4Format (d,c,b,a) =
        show d <> "." <> show c <> "." <> show b <> "." <> show a
  in ipv4Format . bigToLittleEndian $ hostAddressToTuple addr

getFileOffset :: Text -> IO (Maybe Word32)
getFileOffset pathtext =
  do let path = Text.unpack pathtext
     isfile <- doesFileExist path
     case isfile of
       False -> return Nothing
       True ->
         do size <- withFile path ReadMode hFileSize
            if size == 0
              then return Nothing
              else return $ Just (fromIntegral size)
