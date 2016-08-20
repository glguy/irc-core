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
module Client.CApi.Exports
 ( -- * Extension API types
   Glirc_send_message
 , Glirc_print
 , Glirc_list_networks
 , Glirc_list_channels
 , Glirc_list_channel_users
 , Glirc_my_nick
 , Glirc_identifier_cmp
 ) where

import           Client.CApi.Types
import           Client.Message
import           Client.State
import           Client.State.Channel
import           Client.State.Network
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

-- | Dereference the stable pointer passed to extension callbacks
derefToken :: Ptr () -> IO (MVar ClientState)
derefToken = deRefStablePtr . castPtrToStablePtr


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

------------------------------------------------------------------------

-- | Network, command, and parameters are used when transmitting a message.
type Glirc_send_message =
  Ptr ()     {- ^ api token          -} ->
  Ptr FgnMsg {- ^ pointer to message -} ->
  IO CInt    {- ^ 0 on success       -}

foreign export ccall "glirc_send_message" capiSendMessage :: Glirc_send_message

capiSendMessage :: Glirc_send_message
capiSendMessage token msgPtr =
  do mvar    <- derefToken token
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

type Glirc_print =
  Ptr ()  {- ^ api token         -} ->
  CInt    {- ^ enum message_code -} ->
  CString {- ^ message           -} ->
  CSize   {- ^ message length    -} ->
  IO CInt {- ^ 0 on success      -}

foreign export ccall glirc_print :: Glirc_print

glirc_print :: Glirc_print
glirc_print stab code msgPtr msgLen =
  do mvar <- derefToken stab
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

-- | The resulting strings and array of strings are malloc'd and the
-- caller must free them. NULL returned on failure.
type Glirc_list_networks =
  Ptr ()           {- ^ api token                                        -} ->
  IO (Ptr CString) {- ^ null terminated array of null terminated strings -}

foreign export ccall glirc_list_networks :: Glirc_list_networks

glirc_list_networks :: Glirc_list_networks
glirc_list_networks stab =
  do mvar <- derefToken stab
     st   <- readMVar mvar
     let networks = views clientNetworkMap HashMap.keys st
     strs <- traverse (newCString . Text.unpack) networks
     newArray0 nullPtr strs

------------------------------------------------------------------------

type Glirc_identifier_cmp =
  CString {- ^ identifier 1     -} ->
  CSize   {- ^ identifier 1 len -} ->
  CString {- ^ identifier 2     -} ->
  CSize   {- ^ identifier 2 len -} ->
  IO CInt

foreign export ccall glirc_identifier_cmp :: Glirc_identifier_cmp

glirc_identifier_cmp :: Glirc_identifier_cmp
glirc_identifier_cmp p1 n1 p2 n2 =
  do txt1 <- Text.peekCStringLen (p1, fromIntegral n1)
     txt2 <- Text.peekCStringLen (p2, fromIntegral n2)
     return $! case compare (mkId txt1) (mkId txt2) of
                 LT -> -1
                 EQ ->  0
                 GT ->  1

------------------------------------------------------------------------

-- | The resulting strings and array of strings are malloc'd and the
-- caller must free them. NULL returned on failure.
type Glirc_list_channels =
  Ptr ()  {- ^ api token   -} ->
  CString {- ^ network     -} ->
  CSize   {- ^ network len -} ->
  IO (Ptr CString) {- ^ null terminated array of null terminated strings -}

foreign export ccall glirc_list_channels :: Glirc_list_channels

glirc_list_channels :: Glirc_list_channels
glirc_list_channels stab networkPtr networkLen =
  do mvar <- derefToken stab
     st   <- readMVar mvar
     network <- Text.peekCStringLen (networkPtr, fromIntegral networkLen)
     case preview (clientConnection network . csChannels) st of
        Nothing -> return nullPtr
        Just m  ->
          do strs <- traverse (newCString . Text.unpack . idText) (HashMap.keys m)
             newArray0 nullPtr strs

------------------------------------------------------------------------

-- | The resulting strings and array of strings are malloc'd and the
-- caller must free them.  NULL returned on failure.
type Glirc_list_channel_users =
  Ptr ()  {- ^ api token   -} ->
  CString {- ^ network     -} ->
  CSize   {- ^ network len -} ->
  CString {- ^ channel     -} ->
  CSize   {- ^ channel len -} ->
  IO (Ptr CString) {- ^ null terminated array of null terminated strings -}

foreign export ccall glirc_list_channel_users :: Glirc_list_channel_users

glirc_list_channel_users :: Glirc_list_channel_users
glirc_list_channel_users stab networkPtr networkLen channelPtr channelLen =
  do mvar <- derefToken stab
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

-- | The resulting string is malloc'd and the caller must free it.
-- NULL returned on failure.
type Glirc_my_nick =
  Ptr ()  {- ^ api token           -} ->
  CString {- ^ network name        -} ->
  CSize   {- ^ network name length -} ->
  IO CString

foreign export ccall glirc_my_nick :: Glirc_my_nick

glirc_my_nick :: Glirc_my_nick
glirc_my_nick stab networkPtr networkLen =
  do mvar <- derefToken stab
     st   <- readMVar mvar
     network <- Text.peekCStringLen (networkPtr, fromIntegral networkLen)
     let mb = preview (clientConnection network . csNick) st
     case mb of
       Nothing -> return nullPtr
       Just me -> newCString (Text.unpack (idText me))
