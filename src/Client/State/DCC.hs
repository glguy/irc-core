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
    DCCState(..)
  , dsOffers
  , dsTransfers
  -- * DCC offers
  , DCCOffer(..)
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
  , dtStatus
  , ConnectionStatus(..)
  -- * DCC Update
  , DCCUpdate(..)
  -- * Transfer a DCCOffer
  , supervisedDownload
  -- * Parser for DCC request
  , parseSEND
  , parseACCEPT
  -- * DCC RESUME functionality
  , resumeMsg
  , acceptUpdate
  -- * Miscellaneous
  , getFileOffset
  , insertAsNewMax
  , ctcpToTuple
  , statusAtKey
  , reportStopWithStatus
  ) where

import           Control.Applicative (Alternative(..))
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception (bracket, IOException, AsyncException)
import qualified Control.Exception as E
import           Control.Lens hiding (from)
import           Control.Monad (unless, when)
import           Data.Attoparsec.Text
import qualified Data.ByteString as B
import           Data.ByteString.Builder (word32BE, toLazyByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.IntMap (Key, IntMap)
import           Data.IntMap as I hiding (size, empty)
import           Data.List (find)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word32, Word64)
import           Hookup
import           Irc.Identifier (Identifier, idText)
import           Irc.Message (IrcMsg(..))
import           Irc.UserInfo (UserInfo(..), uiNick)
import           Network.Socket ( HostName, PortNumber, Family(..)
                                , hostAddressToTuple )
import           System.Directory (doesFileExist)
import           System.FilePath ((</>))
import           System.IO (withFile, IOMode(..), openFile, hClose, hFileSize)

-- | All the neccesary information to start the download
data DCCOffer = DCCOffer
  { _dccNetwork  :: Text
  , _dccFromInfo :: UserInfo
  , _dccFromIP   :: HostName -- ^ String of the ipv4 representation
  , _dccPort     :: PortNumber
  , _dccFileName :: FilePath
  , _dccSize     :: Word32 -- ^ Size of the whole file, per protocol
                           --   restricted to 32-bits
  , _dccOffset   :: Word32 -- ^ Byte from where the transmission starts
  } deriving (Show, Eq)


makeLenses ''DCCOffer

-- | Status of a connection at certain @Key@
data ConnectionStatus
  = CorrectlyFinished | UserKilled | LostConnection | Downloading
  | Pending | NotExist
  deriving (Eq, Show)

-- | Structure with information of a download accepted via "/dcc accept"
--   or "/dcc resume"
data DCCTransfer = DCCTransfer
  { _dtThread   :: Maybe (Async ()) -- ^ If Nothing, the thread was killed
                                       --   and stopped.
  , _dtProgress :: Word32 -- ^ Percentage of progress
  , _dtStatus   :: ConnectionStatus
  }

makeLenses ''DCCTransfer

-- Check the invariants at @statusAtKey@
data DCCState = DCCState
  { _dsOffers    :: !(IntMap DCCOffer)
  , _dsTransfers :: !(IntMap DCCTransfer)
  }

makeLenses ''DCCState

instance Monoid DCCState where
  mempty = DCCState mempty mempty

instance Semigroup DCCState where
  (DCCState off1 tran1) <> (DCCState off2 tran2) =
      DCCState (off1 <> off2) (tran1 <> tran2)

data DCCUpdate
  = PercentUpdate Key Word32
  | Finished Key
  | SocketInterrupted Key
  | UserInterrupted Key
  | Accept Key PortNumber Word32 -- update that DCC RESUME triggers
  deriving (Show, Eq)            -- Word32 is the offset

-- | Launch a supervisor thread for downloading the offer refered by @Key@ and
--   return the DCCState accordingly.
supervisedDownload :: FilePath -> Key -> TChan DCCUpdate
                   -> DCCState -> IO DCCState
supervisedDownload dir key updChan state = do
  let Just offer = view (dsOffers . at key) state -- Previously check
  supervisorThread <- async $
      withAsync (startDownload dir key updChan offer) $ \realTransferThread ->
        do upd <- E.catches (Finished key <$ wait realTransferThread)
                    [ E.Handler (\(_ :: IOException) ->
                                   return (SocketInterrupted key))
                    -- user killed, thrown by async >= 2.2
                    , E.Handler (\(_ :: AsyncCancelled) ->
                                   return (UserInterrupted key))
                    -- Compatibility hack
                    -- user killed, thrown by async <= 2.1.1 on cancel
                    , E.Handler (\(_ :: AsyncException) ->
                                   return (UserInterrupted key))
                    ]
           atomically $ writeTChan updChan upd
  let startPercent = percent (_dccOffset offer) (_dccSize offer)
      newTransfer = DCCTransfer (Just supervisorThread) startPercent Downloading
      newState = set (dsTransfers . at key) (Just newTransfer) state
  return newState

-- |
startDownload :: FilePath -> Key -> TChan DCCUpdate -> DCCOffer -> IO ()
startDownload dir key updChan offer@(DCCOffer _ _ from port name totalSize offset) = do
  let openMode = if offset > 0 then AppendMode else WriteMode
      filepath = dir </> name
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
           withAsync (report offer key recvChan2 updChan)
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


-- | @send@ing the current size to the fileserver. As an independent
--   acknowledgement stream, it doesn't match the protocol, but matches
--   what other clients and servers do in practice.
sendStream :: Word32 -> Connection -> TChan Word32 -> IO ()
sendStream maxSize conn chan =
  do val <- atomically $ readTChan chan
     let valBE = toStrict . toLazyByteString $ word32BE val
     send conn valBE
     unless (val >= maxSize) (sendStream maxSize conn chan)

-- | Generate @PercentUpdate@ for each percent of download.
report :: DCCOffer -> Key -> TChan Word32 -> TChan DCCUpdate -> IO ()
report offer key input output = compareAndUpdate (percent offset totalsize)
  where
    offset = _dccOffset offer
    totalsize = _dccSize offer

    compareAndUpdate :: Word32 -> IO ()
    compareAndUpdate prevPercent =
      do curSize <- atomically $ readTChan input
         let curPercent = percent (offset + curSize) totalsize
             updateEv   = PercentUpdate key curPercent
         if (curPercent == 100)
           then atomically (writeTChan output updateEv)
           else do when (curPercent > prevPercent)
                        $ atomically (writeTChan output updateEv)
                   compareAndUpdate curPercent

percent :: Word32 -> Word32 -> Word32
percent a total = fromIntegral $
  ((fromIntegral @_ @Word64 a) * 100) `div` (fromIntegral total) -- Avoid overflow

-- | This function can only be called after a @cancel@ has been issued
--   on the supervisor thread at @Key@
reportStopWithStatus :: Key -> ConnectionStatus -> DCCState -> DCCState
reportStopWithStatus key newstatus dccstate =
  let transfer = maybe (error "Wrong index") id $ view (dsTransfers . at key) dccstate
      updatedTransfer = transfer {_dtStatus = newstatus, _dtThread = Nothing}
  in set (dsTransfers . at key) (Just updatedTransfer) dccstate

-- | Parse a "DCC SEND" command.
parseSEND :: Text -> UserInfo -> Text -> Either String DCCOffer
parseSEND network userFrom text = parseOnly (sendFormat network userFrom) text

sendFormat :: Text -> UserInfo -> Parser DCCOffer
sendFormat network userFrom =
  let fun name addr port totalsize =
        DCCOffer network userFrom addr port (Text.unpack name) totalsize 0
  in fun <$> (string "SEND" *> space *> nameFormat)
             <*> (ipv4Dotted <$> (space *> decimal))
             <*> (space *> decimal)
             <*> (space *> decimal)

-- | Parse a "DCC RESUME" command.
parseACCEPT :: DCCState -> UserInfo -> Text -> Maybe DCCUpdate
parseACCEPT state userFrom text =
  let offerList = I.toDescList (_dsOffers state)
      predicate fileName (key, offer)=
          (_dccFileName offer == fileName)
          && userFrom == (_dccFromInfo offer)
          && statusAtKey key state == Pending
  in case parseOnly acceptFormat text of
       Left _ -> Nothing
       Right (fileName, port, offset) ->
         (\(key, _) -> Accept key port offset)
             <$> find (predicate fileName) offerList


acceptFormat :: Parser (FilePath, PortNumber, Word32)
acceptFormat =
  (,,) <$> (string "ACCEPT " *> (Text.unpack <$> nameFormat))
       <*> (space *> decimal) -- port
       <*> (space *> decimal) -- offset

-- Depending on the software, if the filename contains no spaces, the
-- DCC SEND can be sent without a \" enclosing it. Handle that
-- correctly.
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

getFileOffset :: FilePath -> IO (Maybe Word32)
getFileOffset path =
  do isfile <- doesFileExist path
     case isfile of
       False -> return Nothing
       True ->
         do size <- withFile path ReadMode hFileSize
            if size == 0
              then return Nothing
              else return $ Just (fromIntegral size)

insertAsNewMax :: DCCOffer -> DCCState -> DCCState
insertAsNewMax newoffer (DCCState offers transfers) =
  let newmax = if I.null offers then 1 else succ . fst $ I.findMax offers
      newOffers = I.insert newmax newoffer offers
  in DCCState newOffers transfers

-- TODO: maybe replace with some lens magic?
ctcpToTuple :: IrcMsg -> Maybe (UserInfo, Identifier, Text, Text)
ctcpToTuple (Ctcp fromU target command txt) =
  Just (fromU, target, command, txt)
ctcpToTuple (CtcpNotice fromU target command txt) =
  Just (fromU, target, command, txt)
ctcpToTuple _ = Nothing

-- | Check the status of a download at @Key@ by checking the invariants
--   at @DCCState@
statusAtKey :: Key -> DCCState -> ConnectionStatus
statusAtKey key (DCCState offers transfers)
  | Nothing <- lookupOffer = NotExist
  | (Just _) <- lookupOffer, Nothing <- I.lookup key transfers = Pending
  | otherwise = _dtStatus (transfers I.! key)
  where
    lookupOffer = I.lookup key offers

-- | Craft a CTCP message indicating we wan to resume a download at the
--   offset
resumeMsg :: Word32 {- offset -}
          -> DCCOffer
          -> (String, String) -- (target, txt)
resumeMsg sizeoffset offer =
  let filename = _dccFileName offer
      port = show (_dccPort offer)
      sizeoffset' = show sizeoffset
      quoting = if any (== ' ') filename then "\"" else mempty

      txt = "RESUME" <> " " <> quoting <> filename <> quoting <> " " <> port
            <> " " <> sizeoffset'
      target = Text.unpack $ views uiNick idText (_dccFromInfo offer)
  in (target, txt)

-- | Modify the @DCCState@ following the corresponding @DCCUpdate@
acceptUpdate :: DCCUpdate -> DCCState -> DCCState
acceptUpdate (Accept k port offset) state =
  case view (dsOffers . at k) state of
    Nothing -> error "Not a valid key"
    Just oldOffer ->
      let newOffer = oldOffer { _dccPort = port, _dccOffset = offset }
      in set (dsOffers . at k) (Just newOffer) state
acceptUpdate _ state = state
