{-# Language RecordWildCards #-}
{-|
Module      : Client.CApi.Exports
Description : Foreign exports which expose functionality for extensions
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module exports the C functions that extensions can used to query the state
of the client.
-}
module Client.CApi.Exports () where

import           Client.CApi.Types
import           Client.State
import           Client.ChannelState
import           Client.ConnectionState
import           Client.Message
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Lens
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import           Data.Time
import           Foreign.C
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.StablePtr
import           Foreign.Storable
import           Irc.Identifier
import           Irc.RawIrcMsg
import           Irc.UserInfo

------------------------------------------------------------------------

-- | Type stored in the 'StablePtr' passed through the C API
type ApiState = MVar ClientState

------------------------------------------------------------------------

-- | Import a 'FgnMsg' into an 'RawIrcMsg'
peekFgnMsg :: FgnMsg -> IO RawIrcMsg
peekFgnMsg FgnMsg{..} =
  do let strArray n p = traverse peekFgnStringLen =<<
                        peekArray (fromIntegral n) p

     tagKeys <- strArray fmTagN fmTagKeys
     tagVals <- strArray fmTagN fmTagVals
     prefixN  <- peekFgnStringLen fmPrefixNick
     prefixU  <- peekFgnStringLen fmPrefixUser
     prefixH  <- peekFgnStringLen fmPrefixHost
     command <- peekFgnStringLen fmCommand
     params  <- strArray fmParamN fmParams

     return RawIrcMsg
       { _msgTags    = zipWith TagEntry tagKeys tagVals
       , _msgPrefix  = if Text.null prefixN
                         then Nothing
                         else Just (UserInfo (mkId prefixN) prefixU prefixH)
       , _msgCommand = command
       , _msgParams  = params
       }

------------------------------------------------------------------------

-- | Peek a 'FgnStringLen' as UTF-8 encoded bytes.
peekFgnStringLen :: FgnStringLen -> IO Text
peekFgnStringLen (FgnStringLen ptr len) =
  Text.peekCStringLen (ptr, fromIntegral len)

type CApiSendMessage = Ptr () -> Ptr FgnMsg -> IO CInt

foreign export ccall "glirc_send_message" capiSendMessage :: CApiSendMessage

capiSendMessage :: CApiSendMessage
capiSendMessage stPtr msgPtr =
  do mvar    <- deRefStablePtr (castPtrToStablePtr stPtr) :: IO ApiState
     fgn     <- peek msgPtr
     msg     <- peekFgnMsg fgn
     network <- peekFgnStringLen (fmNetwork fgn)
     withMVar mvar $ \st ->
       case preview (clientConnection network) st of
         Nothing -> return 1
         Just cs -> do sendMsg cs msg
                       return 0
  `catch` \SomeException{} -> return 1

------------------------------------------------------------------------

type CApiPrint = Ptr () -> CInt -> CString -> CSize -> IO CInt

foreign export ccall "glirc_print" capiPrint :: CApiPrint

capiPrint :: CApiPrint
capiPrint stPtr code msgPtr msgLen =
  do mvar <- deRefStablePtr (castPtrToStablePtr stPtr) :: IO ApiState
     txt  <- Text.peekCStringLen (msgPtr, fromIntegral msgLen)
     now  <- getZonedTime

     let con | code == normalMessageCode = NormalBody
             | otherwise                 = ErrorBody
         msg = ClientMessage
                 { _msgBody    = con txt
                 , _msgTime    = now
                 , _msgNetwork = Text.empty
                 }
     modifyMVar_ mvar $ \st ->
       do return (recordNetworkMessage msg st)
     return 0
  `catch` \SomeException{} -> return 1

------------------------------------------------------------------------

type CApiListNetworks = Ptr () -> IO (Ptr CString)

foreign export ccall "glirc_list_networks" capiListNetworks :: CApiListNetworks

capiListNetworks :: CApiListNetworks
capiListNetworks stab =
  do mvar <- deRefStablePtr (castPtrToStablePtr stab) :: IO ApiState
     st   <- readMVar mvar
     let networks = views clientNetworkMap HashMap.keys st
     strs <- traverse (newCString . Text.unpack) networks
     newArray0 nullPtr strs

------------------------------------------------------------------------

type CApiIdentifierCmp = CString -> CSize -> CString -> CSize -> IO CInt

foreign export ccall "glirc_identifier_cmp" capiIdentifierCmp :: CApiIdentifierCmp

capiIdentifierCmp :: CApiIdentifierCmp
capiIdentifierCmp p1 n1 p2 n2 =
  do txt1 <- Text.peekCStringLen (p1, fromIntegral n1)
     txt2 <- Text.peekCStringLen (p2, fromIntegral n2)
     return $! case compare (mkId txt1) (mkId txt2) of
                 LT -> -1
                 EQ ->  0
                 GT ->  1

------------------------------------------------------------------------

type CApiListChannels = Ptr () -> CString -> CSize -> IO (Ptr CString)

foreign export ccall "glirc_list_channels" capiListChannels :: CApiListChannels

capiListChannels :: CApiListChannels
capiListChannels stab networkPtr networkLen =
  do mvar <- deRefStablePtr (castPtrToStablePtr stab) :: IO ApiState
     st   <- readMVar mvar
     network <- Text.peekCStringLen (networkPtr, fromIntegral networkLen)
     case preview (clientConnection network . csChannels) st of
        Nothing -> return nullPtr
        Just m  ->
          do strs <- traverse (newCString . Text.unpack . idText) (HashMap.keys m)
             newArray0 nullPtr strs

------------------------------------------------------------------------

type CApiListChannelUsers = Ptr () -> CString -> CSize -> CString -> CSize -> IO (Ptr CString)

foreign export ccall "glirc_list_channel_users" capiListChannelUsers :: CApiListChannelUsers

capiListChannelUsers :: CApiListChannelUsers
capiListChannelUsers stab networkPtr networkLen channelPtr channelLen =
  do mvar <- deRefStablePtr (castPtrToStablePtr stab) :: IO ApiState
     st   <- readMVar mvar
     network <- Text.peekCStringLen (networkPtr, fromIntegral networkLen)
     channel <- Text.peekCStringLen (channelPtr, fromIntegral channelLen)
     let mb = preview ( clientConnection network
                      . csChannels . ix (mkId channel)
                      . chanUsers
                      ) st
     case mb of
       Nothing -> return nullPtr
       Just m  ->
         do strs <- traverse (newCString . Text.unpack . idText) (HashMap.keys m)
            newArray0 nullPtr strs

------------------------------------------------------------------------

type CApiMyNick =
  Ptr () ->
  CString {- ^ network name        -} ->
  CSize   {- ^ network name length -} ->
  IO CString

foreign export ccall "glirc_my_nick" capiMyNick :: CApiMyNick

capiMyNick :: CApiMyNick
capiMyNick stab networkPtr networkLen =
  do mvar <- deRefStablePtr (castPtrToStablePtr stab) :: IO ApiState
     st   <- readMVar mvar
     network <- Text.peekCStringLen (networkPtr, fromIntegral networkLen)
     let mb = preview (clientConnection network . csNick) st
     case mb of
       Nothing -> return nullPtr
       Just me -> newCString (Text.unpack (idText me))
