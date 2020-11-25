{-# Language RecordWildCards #-}
{-|
Module      : Client.CApi.Exports
Description : Foreign exports which expose functionality for extensions
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module exports the C functions that extensions can used to query the state
of the client.

C Extensions can include @glirc-api.h@
-}
module Client.CApi.Exports
 ( -- * Extension entry-points
   Glirc_send_message
 , glirc_send_message

 , Glirc_print
 , glirc_print

 , Glirc_list_networks
 , glirc_list_networks

 , Glirc_list_channels
 , glirc_list_channels

 , Glirc_list_channel_users
 , glirc_list_channel_users

 , Glirc_my_nick
 , glirc_my_nick

 , Glirc_user_account
 , glirc_user_account

 , Glirc_user_channel_modes
 , glirc_user_channel_modes

 , Glirc_channel_modes
 , glirc_channel_modes

 , Glirc_channel_masks
 , glirc_channel_masks

 , Glirc_identifier_cmp
 , glirc_identifier_cmp

 , Glirc_is_channel
 , glirc_is_channel

 , Glirc_is_logged_on
 , glirc_is_logged_on

 , Glirc_mark_seen
 , glirc_mark_seen

 , Glirc_clear_window
 , glirc_clear_window

 , Glirc_current_focus
 , glirc_current_focus

 , Glirc_set_focus
 , glirc_set_focus

 , Glirc_free_string
 , glirc_free_string

 , Glirc_free_strings
 , glirc_free_strings

 , Glirc_inject_chat
 , glirc_inject_chat

 , Glirc_resolve_path
 , glirc_resolve_path

 , Glirc_set_timer
 , glirc_set_timer

 , Glirc_cancel_timer
 , glirc_cancel_timer

 , Glirc_window_lines
 , glirc_window_lines

 , Glirc_thread
 , glirc_thread
 ) where

import           Client.CApi (cancelTimer, pushTimer, ThreadEntry(..), ActiveExtension(aeThreads))
import           Client.CApi.Types
import           Client.Configuration
import           Client.Message
import           Client.State
import           Client.State.Channel
import           Client.State.Focus
import           Client.State.Network
import           Client.State.Window
import           Client.UserHost
import           Control.Concurrent (forkOS)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM (atomically, writeTQueue)
import           Control.Exception
import           Control.Lens
import           Control.Monad (unless)
import           Data.Char (chr)
import           Data.Foldable (traverse_)
import           Data.Functor.Compose
import qualified Data.Map as Map
import           Data.Monoid (First(..))
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
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
import           Irc.Message
import           LensUtils

------------------------------------------------------------------------

-- | Dereference the stable pointer passed to extension callbacks
derefToken :: Ptr () -> IO (MVar (Int, ClientState))
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

-- | Type of 'glirc_send_message' extension entry-point
type Glirc_send_message =
  Ptr ()     {- ^ api token          -} ->
  Ptr FgnMsg {- ^ pointer to message -} ->
  IO CInt    {- ^ 0 on success       -}

-- | Entry-point into the client when an extern extension wants send an IRC
-- command to a connected server.
glirc_send_message :: Glirc_send_message
glirc_send_message token msgPtr =
  do mvar    <- derefToken token
     fgn     <- peek msgPtr
     msg     <- peekFgnMsg fgn
     network <- peekFgnStringLen (fmNetwork fgn)
     (_,st)  <- readMVar mvar
     case preview (clientConnection network) st of
       Nothing -> return 1
       Just cs -> 0 <$ sendMsg cs msg
  `catch` \SomeException{} -> return 1

------------------------------------------------------------------------

-- | Type of 'glirc_print' extension entry-point
type Glirc_print =
  Ptr ()      {- ^ api token         -} ->
  MessageCode {- ^ enum message_code -} ->
  CString     {- ^ message           -} ->
  CSize       {- ^ message length    -} ->
  IO CInt     {- ^ 0 on success      -}

-- | Entry-point for extensions to append a message to the client buffer.
-- The @message_code@ can be used to render the message normally or to
-- cause the client to draw attention to the message as an error.
glirc_print :: Glirc_print
glirc_print stab code msgPtr msgLen =
  do mvar <- derefToken stab
     txt  <- peekFgnStringLen (FgnStringLen msgPtr msgLen)
     now  <- getZonedTime

     let con | code == normalMessage = NormalBody
             | otherwise             = ErrorBody
         msg = ClientMessage
                 { _msgBody    = con txt
                 , _msgTime    = now
                 , _msgNetwork = Text.empty
                 }
     modifyMVar_ mvar $ \(i,st) ->
       do return (i, recordNetworkMessage msg st)
     return 0
  `catch` \SomeException{} -> return 1

------------------------------------------------------------------------

-- | Type of 'glirc_inject_chat' extension entry-point.
type Glirc_inject_chat =
  Ptr ()  {- ^ api token         -} ->
  CString {- ^ network           -} ->
  CSize   {- ^ network length    -} ->
  CString {- ^ source            -} ->
  CSize   {- ^ source length     -} ->
  CString {- ^ target            -} ->
  CSize   {- ^ target length     -} ->
  CString {- ^ message           -} ->
  CSize   {- ^ message length    -} ->
  IO CInt {- ^ 0 on success      -}

-- | Add a message to a chat window as if it was received
-- directly from the IRC server. This is useful when implementing
-- extensions that intercept incoming chat messages and transform
-- them before showing the user.
glirc_inject_chat :: Glirc_inject_chat
glirc_inject_chat stab netPtr netLen srcPtr srcLen tgtPtr tgtLen msgPtr msgLen =
  do mvar <- derefToken stab
     net  <- peekFgnStringLen (FgnStringLen netPtr netLen)
     src  <- peekFgnStringLen (FgnStringLen srcPtr srcLen)
     tgt  <- mkId <$> peekFgnStringLen (FgnStringLen tgtPtr tgtLen)
     txt  <- peekFgnStringLen (FgnStringLen msgPtr msgLen)
     now  <- getZonedTime

     let msg = ClientMessage
                 { _msgBody    = IrcBody (Privmsg (parseUserInfo src) tgt txt)
                 , _msgTime    = now
                 , _msgNetwork = net
                 }
     modifyMVar_ mvar $ \(i, st) ->
       do return (i, recordChannelMessage net tgt msg st)
     return 0
  `catch` \SomeException{} -> return 1

------------------------------------------------------------------------

-- | Type of 'glirc_list_networks' extension entry-point
type Glirc_list_networks =
  Ptr ()           {- ^ api token                                        -} ->
  IO (Ptr CString) {- ^ null terminated array of null terminated strings -}

-- | This extension entry-point allocates a list of all the identifiers for
-- the active networks on the client. @NULL@ returned on failure.
-- The caller is responsible for freeing successful result with
-- @glirc_free_strings@.
glirc_list_networks :: Glirc_list_networks
glirc_list_networks stab =
  do mvar <- derefToken stab
     (_,st) <- readMVar mvar
     let networks = views clientConnections HashMap.keys st
     strs <- traverse (newCString . Text.unpack) networks
     newArray0 nullPtr strs

------------------------------------------------------------------------

-- | Type of 'glirc_identifier_cmp' extension entry-point
type Glirc_identifier_cmp =
  CString {- ^ identifier 1     -} ->
  CSize   {- ^ identifier 1 len -} ->
  CString {- ^ identifier 2     -} ->
  CSize   {- ^ identifier 2 len -} ->
  IO CInt

-- | Case insensitive comparison suitable for use on channels and nicknames.
-- Returns -1 if the first identifier is less than the second
-- Returns 0 if the first identifier is equal to the second
-- Returns 1 if the first identifier is greater than the second
glirc_identifier_cmp :: Glirc_identifier_cmp
glirc_identifier_cmp p1 n1 p2 n2 =
  do txt1 <- peekFgnStringLen (FgnStringLen p1 n1)
     txt2 <- peekFgnStringLen (FgnStringLen p2 n2)
     return $! case compare (mkId txt1) (mkId txt2) of
                 LT -> -1
                 EQ ->  0
                 GT ->  1

------------------------------------------------------------------------

-- | Type of 'glirc_list_channels' extension entry-point
type Glirc_list_channels =
  Ptr ()  {- ^ api token   -} ->
  CString {- ^ network     -} ->
  CSize   {- ^ network len -} ->
  IO (Ptr CString) {- ^ null terminated array of null terminated strings -}

-- | Generate a list of connected channels for the network identified in
-- the arguments. @NULL@ returned on failure. Caller is responsible for
-- freeing successful result with @glirc_free_strings@.
glirc_list_channels :: Glirc_list_channels
glirc_list_channels stab networkPtr networkLen =
  do mvar <- derefToken stab
     (_,st) <- readMVar mvar
     network <- peekFgnStringLen (FgnStringLen networkPtr networkLen)
     case preview (clientConnection network . csChannels) st of
        Nothing -> return nullPtr
        Just m  ->
          do strs <- traverse (newCString . Text.unpack . idText) (HashMap.keys m)
             newArray0 nullPtr strs

------------------------------------------------------------------------

-- | Type of 'glirc_list_channel_users' extension entry-point
type Glirc_list_channel_users =
  Ptr ()  {- ^ api token   -} ->
  CString {- ^ network     -} ->
  CSize   {- ^ network len -} ->
  CString {- ^ channel     -} ->
  CSize   {- ^ channel len -} ->
  IO (Ptr CString) {- ^ null terminated array of null terminated strings -}

-- | Generate a list of IRC nicknames currently connected to the identified
-- channel on the identified network. @NULL@ returned on failure.
-- Caller is responsible for freeing successful result with
-- @glirc_free_strings@.
glirc_list_channel_users :: Glirc_list_channel_users
glirc_list_channel_users stab networkPtr networkLen channelPtr channelLen =
  do mvar    <- derefToken stab
     (_, st) <- readMVar mvar
     network <- peekFgnStringLen (FgnStringLen networkPtr networkLen)
     channel <- peekFgnStringLen (FgnStringLen channelPtr channelLen)
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

-- | Type of 'glirc_my_nick' extension entry-point
type Glirc_my_nick =
  Ptr ()  {- ^ api token           -} ->
  CString {- ^ network name        -} ->
  CSize   {- ^ network name length -} ->
  IO CString

-- | Return the IRC nickname associated with the active network
-- connection identified in the arguments. @NULL@ returned on failure.
-- Caller is responsible for freeing successful result with
-- @glirc_free_string@.
glirc_my_nick :: Glirc_my_nick
glirc_my_nick stab networkPtr networkLen =
  do mvar    <- derefToken stab
     (_,st)  <- readMVar mvar
     network <- peekFgnStringLen (FgnStringLen networkPtr networkLen)
     let mb = preview (clientConnection network . csNick) st
     case mb of
       Nothing -> return nullPtr
       Just me -> newCString (Text.unpack (idText me))

------------------------------------------------------------------------

-- | Type of 'glirc_user_account' extension entry-point
type Glirc_user_account =
  Ptr ()  {- ^ api token           -} ->
  CString {- ^ network name        -} ->
  CSize   {- ^ network name length -} ->
  CString {- ^ nickname            -} ->
  CSize   {- ^ nickname length     -} ->
  IO CString

-- | Return the services account name associated with a nickname on
-- a server as tracked by the client. Caller is responsible for freeing
-- successful result with @glirc_free_string@. If no account is
-- known, @NULL@ is returned.
glirc_user_account :: Glirc_user_account
glirc_user_account stab networkPtr networkLen nickPtr nickLen =
  do mvar    <- derefToken stab
     (_,st)  <- readMVar mvar
     network <- peekFgnStringLen (FgnStringLen networkPtr networkLen)
     nick    <- peekFgnStringLen (FgnStringLen nickPtr    nickLen   )
     let mb = preview ( clientConnection network
                      . csUsers . ix (mkId nick)
                      . uhAccount . filtered (not . Text.null)) st
     case mb of
       Just acct -> newCString (Text.unpack acct)
       _         -> return nullPtr

------------------------------------------------------------------------

-- | Type of 'glirc_user_account' extension entry-point
type Glirc_user_channel_modes =
  Ptr ()  {- ^ api token           -} ->
  CString {- ^ network name        -} ->
  CSize   {- ^ network name length -} ->
  CString {- ^ channel             -} ->
  CSize   {- ^ channel length      -} ->
  CString {- ^ nickname            -} ->
  CSize   {- ^ nickname length     -} ->
  IO CString

-- | Return the sigils associated with a nickname on a particular channel.
-- Caller is responsible for freeing successful result with
-- @glirc_free_string@. If user is on channel without any sigils an empty
-- string is returned. If user is not on channel @NULL@ is returned.
glirc_user_channel_modes :: Glirc_user_channel_modes
glirc_user_channel_modes stab netPtr netLen chanPtr chanLen nickPtr nickLen =
  do mvar    <- derefToken stab
     (_,st)  <- readMVar mvar
     network <- peekFgnStringLen (FgnStringLen netPtr  netLen)
     chan    <- peekFgnStringLen (FgnStringLen chanPtr chanLen   )
     nick    <- peekFgnStringLen (FgnStringLen nickPtr nickLen   )
     let mb = preview ( clientConnection network
                      . csChannels . ix (mkId chan)
                      . chanUsers  . ix (mkId nick) ) st
     case mb of
       Just sigils -> newCString sigils
       Nothing     -> return nullPtr

------------------------------------------------------------------------

-- | Type of 'glirc_channel_modes' extension entry-point
type Glirc_channel_modes =
  Ptr ()  {- ^ api token           -} ->
  CString {- ^ network name        -} ->
  CSize   {- ^ network name length -} ->
  CString {- ^ channel             -} ->
  CSize   {- ^ channel length      -} ->
  IO (Ptr CString)

-- | Return all of the modes of a given channel. The first
-- letter of each string returned is the mode. Any remaining
-- characters are the mode argument.
-- Caller is responsible for freeing successful result with
-- @glirc_free_strings@. If the user is not on a channel @NULL@
-- is returned. The modes might not be known to the client for
-- a particular channel which can result in an empty list of
-- modes being returned.
glirc_channel_modes :: Glirc_channel_modes
glirc_channel_modes stab netPtr netLen chanPtr chanLen =
  do mvar    <- derefToken stab
     (_,st)  <- readMVar mvar
     network <- peekFgnStringLen (FgnStringLen netPtr  netLen)
     chan    <- peekFgnStringLen (FgnStringLen chanPtr chanLen   )
     let mb = preview ( clientConnection network
                      . csChannels . ix (mkId chan)
                      . chanModes
                      ) st
     case mb of
       Nothing -> return nullPtr
       Just modeMap ->
         do let strings = [ mode : Text.unpack arg | (mode,arg) <- Map.toList modeMap ]
            strs <- traverse newCString strings
            newArray0 nullPtr strs

------------------------------------------------------------------------

-- | Type of 'glirc_channel_masks' extension entry-point
type Glirc_channel_masks =
  Ptr ()  {- ^ api token           -} ->
  CString {- ^ network name        -} ->
  CSize   {- ^ network name length -} ->
  CString {- ^ channel             -} ->
  CSize   {- ^ channel length      -} ->
  CChar   {- ^ mode                -} ->
  IO (Ptr CString)

-- | Return all of the modes of a given channel. The first
-- letter of each string returned is the mode. Any remaining
-- characters are the mode argument.
-- Caller is responsible for freeing successful result with
-- @glirc_free_strings@. If the user is not on a channel @NULL@
-- is returned. The modes might not be known to the client for
-- a particular channel which can result in an empty list of
-- modes being returned.
glirc_channel_masks :: Glirc_channel_masks
glirc_channel_masks stab netPtr netLen chanPtr chanLen cmode =
  do let mode = chr (fromIntegral cmode) :: Char
     mvar    <- derefToken stab
     (_,st)  <- readMVar mvar
     network <- peekFgnStringLen (FgnStringLen netPtr  netLen)
     chan    <- peekFgnStringLen (FgnStringLen chanPtr chanLen   )
     let mb = preview ( clientConnection network
                      . csChannels . ix (mkId chan)
                      . chanLists  . ix mode
                      ) st
     case mb of
       Nothing -> return nullPtr
       Just listMap ->
         do strs <- traverse (newCString . Text.unpack) (HashMap.keys listMap)
            newArray0 nullPtr strs

------------------------------------------------------------------------

-- | Type of 'glirc_mark_seen' extension entry-point
type Glirc_mark_seen =
  Ptr ()  {- ^ api token           -} ->
  CString {- ^ network name        -} ->
  CSize   {- ^ network name length -} ->
  CString {- ^ channel name        -} ->
  CSize   {- ^ channel name length -} ->
  IO ()

-- | Mark a window as being seen, clearing the new message counter.
-- To specify the client window send an empty network name.
-- To specify a network window send an empty channel name.
glirc_mark_seen :: Glirc_mark_seen
glirc_mark_seen stab networkPtr networkLen channelPtr channelLen =
  do network <- peekFgnStringLen (FgnStringLen networkPtr networkLen)
     channel <- peekFgnStringLen (FgnStringLen channelPtr channelLen)

     let focus
           | Text.null network = Unfocused
           | Text.null channel = NetworkFocus network
           | otherwise         = ChannelFocus network (mkId channel)

     mvar <- derefToken stab
     modifyMVar_ mvar $ \(i,st) ->
       let st' = overStrict (clientWindows . ix focus) windowSeen st
       in st' `seq` return (i,st')

------------------------------------------------------------------------

-- | Type of 'glirc_clear_window' extension entry-point
type Glirc_clear_window =
  Ptr ()  {- ^ api token           -} ->
  CString {- ^ network name        -} ->
  CSize   {- ^ network name length -} ->
  CString {- ^ channel name        -} ->
  CSize   {- ^ channel name length -} ->
  IO ()

-- | Clear contents of a specified window.
-- To specify the client window send an empty network name.
-- To specify a network window send an empty channel name.
glirc_clear_window :: Glirc_clear_window
glirc_clear_window stab networkPtr networkLen channelPtr channelLen =
  do network <- peekFgnStringLen (FgnStringLen networkPtr networkLen)
     channel <- peekFgnStringLen (FgnStringLen channelPtr channelLen)

     let focus
           | Text.null network = Unfocused
           | Text.null channel = NetworkFocus network
           | otherwise         = ChannelFocus network (mkId channel)

     mvar <- derefToken stab
     modifyMVar_ mvar $ \(i,st) ->
       let st' = set (clientWindows . ix focus) emptyWindow st
       in st' `seq` return (i,st')

------------------------------------------------------------------------

-- | Type of 'glirc_free_string' extension entry-point
type Glirc_free_string =
  CString {- ^ glirc allocated string -} ->
  IO ()

-- | Free one of the heap allocated strings found as a return value
-- from the extension API. If argument is @NULL@, nothing happens.
glirc_free_string :: Glirc_free_string
glirc_free_string = free

------------------------------------------------------------------------

-- | Type of 'glirc_free_strings' extension entry-point
type Glirc_free_strings =
  Ptr CString {- ^ glirc allocated strings, null-terminated -} ->
  IO ()

-- | Free an array of heap allocated strings found as a return value
-- from the extension API. If argument is @NULL@, nothing happens.
glirc_free_strings :: Glirc_free_strings
glirc_free_strings p =
  unless (p == nullPtr) $
    do traverse_ free =<< peekArray0 nullPtr p
       free p

------------------------------------------------------------------------

-- | Type of 'glirc_current_focus' extension entry-point
type Glirc_current_focus =
  Ptr ()      {- ^ api token                           -} ->
  Ptr CString {- ^ OUT: newly allocated network string -} ->
  Ptr CSize   {- ^ OUT: network length                 -} ->
  Ptr CString {- ^ OUT: newly allocated target string  -} ->
  Ptr CSize   {- ^ OUT: target length                  -} ->
  IO ()

-- | Find the network and target identifier associated with the
-- currently focused window.
--
-- Free the allocated strings with @glirc_free_string@.
--
-- Strings set to @NULL@ if there is no current network or no
-- current target.
glirc_current_focus :: Glirc_current_focus
glirc_current_focus stab netP netL tgtP tgtL =
  do mvar   <- derefToken stab
     (_,st) <- readMVar mvar
     let (net,tgt) = case view clientFocus st of
                       Unfocused        -> (Text.empty, Text.empty)
                       NetworkFocus n   -> (n         , Text.empty)
                       ChannelFocus n t -> (n         , idText t  )
     exportText netP netL net
     exportText tgtP tgtL tgt

------------------------------------------------------------------------

-- | Type of 'glirc_set_focus' extension entry-point
type Glirc_set_focus =
  Ptr ()  {- ^ api token      -} ->
  CString {- ^ network        -} ->
  CSize   {- ^ network length -} ->
  CString {- ^ target         -} ->
  CSize   {- ^ target length  -} ->
  IO ()

-- | Assign window focus to a new value.
--
-- Set to client window if network is empty.
--
-- Set to network window if channel is empty.
--
-- Set to chat window otherwise.
glirc_set_focus :: Glirc_set_focus
glirc_set_focus stab netP netL tgtP tgtL =
  do mvar   <- derefToken stab
     net    <- peekFgnStringLen (FgnStringLen netP netL)
     tgt    <- peekFgnStringLen (FgnStringLen tgtP tgtL)

     let focus
           | Text.null net = Unfocused
           | Text.null tgt = NetworkFocus net
           | otherwise     = ChannelFocus net (mkId tgt)

     modifyMVar_ mvar $ \(i,st) ->
       let st' = changeFocus focus st
       in st' `seq` return (i,st')

------------------------------------------------------------------------

-- | Type of 'glirc_is_channel' extension entry-point
type Glirc_is_channel =
  Ptr ()  {- ^ api token       -} ->
  CString {- ^ network name    -} ->
  CSize   {- ^ network length  -} ->
  CString {- ^ target name     -} ->
  CSize   {- ^ target length   -} ->
  IO CInt {- ^ boolean         -}

-- | Returns @1@ when the given target on the given network is a channel
-- name, otherwise returns @0@.
--
-- If the given network is not currently active this returns @0@.
glirc_is_channel :: Glirc_is_channel
glirc_is_channel stab net netL tgt tgtL =
  do mvar    <- derefToken stab
     (_,st)  <- readMVar mvar
     network <- peekFgnStringLen (FgnStringLen net netL)
     target  <- peekFgnStringLen (FgnStringLen tgt tgtL)

     case preview (clientConnection network) st of
       Just cs | isChannelIdentifier cs (mkId target) -> return 1
       _ -> return 0

------------------------------------------------------------------------

-- | Type of 'glirc_is_logged_on' extension entry-point
type Glirc_is_logged_on =
  Ptr ()  {- ^ api token       -} ->
  CString {- ^ network name    -} ->
  CSize   {- ^ network length  -} ->
  CString {- ^ target name     -} ->
  CSize   {- ^ target length   -} ->
  IO CInt {- ^ boolean         -}

-- | Returns @1@ when the given target on the given network shares a
-- channel with the user, @0@ otherwise.
--
-- If the given network is not currently active this returns @0@.
glirc_is_logged_on :: Glirc_is_logged_on
glirc_is_logged_on stab net netL tgt tgtL =
  do mvar    <- derefToken stab
     (_,st)  <- readMVar mvar
     network <- peekFgnStringLen (FgnStringLen net netL)
     target  <- peekFgnStringLen (FgnStringLen tgt tgtL)

     let online = has (clientConnection network . csUsers . ix (mkId target)) st
     return $! if online then 1 else 0

------------------------------------------------------------------------

-- | Type of 'glirc_resolve_path' extension entry-point
type Glirc_resolve_path =
  Ptr ()     {- ^ api token     -} ->
  CString    {- ^ path          -} ->
  CSize      {- ^ path length   -} ->
  IO CString {- ^ resolved path -}

-- | Resolve the given string to an absolute path. This expands @~@ for
-- the home directory and computes paths relative to the configuration
-- file.
--
-- Free the allocated string with @glirc_free_string@.
glirc_resolve_path :: Glirc_resolve_path
glirc_resolve_path stab pathP pathL =
  do mvar    <- derefToken stab
     (_,st)  <- readMVar mvar
     path    <- peekFgnStringLen (FgnStringLen pathP pathL)

     let cfgPath = view clientConfigPath st
     cxt <- newFilePathContext cfgPath
     newCString (resolveFilePath cxt (Text.unpack path))

------------------------------------------------------------------------

-- | Type of 'glirc_set_timer' extension entry-point
type Glirc_set_timer =
  Ptr ()               {- ^ api token          -} ->
  CULong               {- ^ milliseconds delay -} ->
  FunPtr TimerCallback {- ^ function           -} ->
  Ptr ()               {- ^ callback state     -} ->
  IO TimerId           {- ^ timer ID           -}

-- | Register a function to be called after a given number of milliseconds
-- of delay. The returned timer ID can be used to cancel the timer.
glirc_set_timer :: Glirc_set_timer
glirc_set_timer stab millis fun ptr =
  do mvar    <- derefToken stab
     time    <- addUTCTime (fromIntegral millis / 1000) <$> getCurrentTime
     modifyMVar mvar $ \(i,st) ->
       let (timer,st') = st & clientExtensions . esActive . singular (ix i)
                            %%~ pushTimer time fun ptr
       in st' `seq` return ((i,st'), fromIntegral timer)

------------------------------------------------------------------------

-- | Type of 'glirc_cancel_timer' extension entry-point
type Glirc_cancel_timer =
  Ptr ()               {- ^ api token                   -} ->
  TimerId              {- ^ timer ID                    -} ->
  IO (Ptr ())          {- ^ returns held callback state -}

-- | Register a function to be called after a given number of milliseconds
-- of delay. The returned timer ID can be used to cancel the timer.
glirc_cancel_timer :: Glirc_cancel_timer
glirc_cancel_timer stab timerId =
  do mvar <- derefToken stab
     modifyMVar mvar $ \(i,st) ->
       let Compose mb = st & clientExtensions . esActive . ix i
                   %%~ \ae -> Compose $
                              do (entry, ae') <- cancelTimer (fromIntegral timerId) ae
                                 return (First (Just entry), ae')
       in return $! case mb of
            Just (First (Just ptr), st') -> ((i,st'), ptr)
            _ -> ((i, st), nullPtr)

------------------------------------------------------------------------

-- | Type of 'glirc_window_lines' extension entry-point
type Glirc_window_lines =
  Ptr ()  {- ^ api token       -} ->
  CString {- ^ network name    -} ->
  CSize   {- ^ network length  -} ->
  CString {- ^ target name     -} ->
  CSize   {- ^ target length   -} ->
  CInt    {- ^ use filter      -} ->
  IO (Ptr CString) {- ^ null terminated array of null terminated strings -}

-- | This extension entry-point allocates a list of all the window lines for
-- the requested window. The lines are presented with newest line at the head
-- of the list.
-- The caller is responsible for freeing successful result with
-- @glirc_free_strings@.
glirc_window_lines :: Glirc_window_lines
glirc_window_lines stab net netL tgt tgtL filt =
  do mvar <- derefToken stab
     (_,st) <- readMVar mvar
     network <- peekFgnStringLen (FgnStringLen net netL)
     channel <- peekFgnStringLen (FgnStringLen tgt tgtL)
     let focus
           | Text.null network = Unfocused
           | Text.null channel = NetworkFocus network
           | otherwise         = ChannelFocus network (mkId channel)
         filterFun
           | filt == 0 = id
           | otherwise = clientFilter st id
         strs = toListOf (clientWindows . ix focus . winMessages . each . wlText) st
     ptrs <- traverse (newCString . LText.unpack) (filterFun strs)
     newArray0 nullPtr ptrs

------------------------------------------------------------------------

-- | Type of 'glirc_thread' extension entry-point
type Glirc_thread =
  Ptr ()  {- ^ api token -} ->
  FunPtr (Ptr () -> IO (Ptr ())) {- ^ start -} ->
  FunPtr (Ptr () -> IO ()) {- ^ finish -} ->
  Ptr () {- ^ start argument  -} ->
  IO ()  {- ^ null terminated array of null terminated strings -}

-- | This extension entry-point allocates a list of all the window lines for
-- the requested window. The lines are presented with newest line at the head
-- of the list.
-- The caller is responsible for freeing successful result with
-- @glirc_free_strings@.
glirc_thread :: Glirc_thread
glirc_thread stab start finish arg =
  do mvar <- derefToken stab
     modifyMVar_ mvar $ \(i,st) ->
       do _ <- forkOS $
            do result <- runThreadStart start arg
               atomically (writeTQueue (view clientThreadJoins st) (i, ThreadEntry finish result))
          let incThreads ae = ae { aeThreads = aeThreads ae + 1}
          pure (i, over (clientExtensions . esActive . ix i) incThreads st)
