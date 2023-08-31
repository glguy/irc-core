{-# Language RecordWildCards, ImportQualifiedPost #-}

{-|
Module      : Client.CApi.Types
Description : Marshaling support for C API
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

Marshaling types and functions for the C API

-}

#include "glirc-api.h"

module Client.CApi.Types
  ( -- * Extension record
    FgnExtension(..)
  , StartExtension
  , StopExtension
  , ProcessMessage
  , ProcessCommand
  , ProcessChat
  , TimerCallback
  , TimerId
  , ThreadFinish

  -- * Strings
  , FgnStringLen(..)

  -- * Messages
  , FgnMsg(..)

  -- * Commands
  , FgnCmd(..)

  -- * Chat
  , FgnChat(..)

  -- * Function pointer calling
  , Dynamic
  , runStartExtension
  , runStopExtension
  , runProcessMessage
  , runProcessCommand
  , runProcessChat
  , runTimerCallback
  , runThreadStart
  , runThreadFinish

  -- * report message codes
  , MessageCode(..), normalMessage, errorMessage

  -- * process message results
  , ProcessResult(..), passMessage, dropMessage

  -- * Marshaling helpers
  , withText0
  , exportText
  , poke'
  ) where

import Control.Monad
import Data.Int
import Data.Text (Text)
import Data.Text.Foreign qualified as Text
import Data.Word
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

-- | Tag for describing the kind of message to display in the client
-- as used in `glirc_print`. See 'normalMessage' and 'errorMessage'.
--
-- @enum message_code;@
newtype MessageCode = MessageCode (#type enum message_code) deriving Eq

-- | Normal client message. Unread counter increments, but no client
-- bell or error status update.
normalMessage :: MessageCode
normalMessage = MessageCode (#const NORMAL_MESSAGE)

-- | Important client message. Unread counter increments, bell rings,
-- and error status updates.
errorMessage :: MessageCode
errorMessage = MessageCode (#const ERROR_MESSAGE)

-- | Result used to determine what to do after processing a message with
-- the 'ProcessMessage' callback.
--
-- | @enum process_result@
newtype ProcessResult = ProcessResult (#type enum process_result) deriving Eq

-- | Allow the message to proceed through the client logic.
passMessage :: ProcessResult
passMessage = ProcessResult (#const PASS_MESSAGE)

-- | Drop the message from further processing.
dropMessage :: ProcessResult
dropMessage = ProcessResult (#const DROP_MESSAGE)

-- | @typedef void *start(void *glirc, const char *path)@
type StartExtension =
  Ptr ()           {- ^ api token                   -} ->
  CString          {- ^ path to extension           -} ->
  Ptr FgnStringLen {- ^ array of arguments          -} ->
  CSize            {- ^ number of arguments         -} ->
  IO (Ptr ())      {- ^ initialized extension state -}

-- | @typedef void stop(void *glirc, void *S)@
type StopExtension =
  Ptr () {- ^ extension state -} ->
  IO ()

-- | @typedef enum process_result process_message(void *glirc, void *S, const struct glirc_message *)@
type ProcessMessage =
  Ptr ()     {- ^ extention state -} ->
  Ptr FgnMsg {- ^ message to send -} ->
  IO ProcessResult

-- | @typedef void process_command(void *glirc, void *S, const struct glirc_command *)@
type ProcessCommand =
  Ptr ()     {- ^ extension state -} ->
  Ptr FgnCmd {- ^ command         -} ->
  IO ()

-- | @typedef void process_chat(void *glirc, void *S, const struct glirc_chat *)@
type ProcessChat =
  Ptr ()      {- ^ extension state -} ->
  Ptr FgnChat {- ^ chat info       -} ->
  IO ProcessResult

-- | Integer type of timer IDs
type TimerId = #type timer_id

-- | Callback function when timer triggers
type TimerCallback =
  Ptr ()  {- ^ timer state     -} ->
  TimerId {- ^ timer ID        -} ->
  IO ()

-- | Startup function for threads
type ThreadStart =
  Ptr () {- ^ initial argument -} ->
  IO (Ptr ()) {- ^ result for ThreadFinish -}

-- | @typedef void thread_finish(void *glirc, void *S)@
type ThreadFinish =
  Ptr () {- ^ thread result -} ->
  IO ()

-- | Type of dynamic function pointer wrappers. These convert C
-- function-pointers into Haskell functions.
type Dynamic a = FunPtr a -> a

-- | Dynamic import for 'StartExtension'.
foreign import ccall "dynamic" runStartExtension :: Dynamic StartExtension
-- | Dynamic import for 'StopExtension'.
foreign import ccall "dynamic" runStopExtension  :: Dynamic StopExtension
-- | Dynamic import for 'ProcessMessage'.
foreign import ccall "dynamic" runProcessMessage :: Dynamic ProcessMessage
-- | Dynamic import for 'ProcessCommand'.
foreign import ccall "dynamic" runProcessCommand :: Dynamic ProcessCommand
-- | Dynamic import for 'ProcessChat'.
foreign import ccall "dynamic" runProcessChat    :: Dynamic ProcessChat
-- | Dynamic import for timer callback
foreign import ccall "dynamic" runTimerCallback  :: Dynamic TimerCallback
-- | Dynamic import for thread starts
foreign import ccall "dynamic" runThreadStart    :: Dynamic ThreadStart
-- | Dynamic import for 'ThreadFinish'.
foreign import ccall "dynamic" runThreadFinish   :: Dynamic ThreadFinish

------------------------------------------------------------------------

-- | Information describing an extension's entry-points and metadata.
data FgnExtension = FgnExtension
  { fgnStart   :: FunPtr StartExtension -- ^ Optional startup callback
  , fgnStop    :: FunPtr StopExtension  -- ^ Optional shutdown callback
  , fgnMessage :: FunPtr ProcessMessage -- ^ Optional message received callback
  , fgnChat    :: FunPtr ProcessChat    -- ^ Optional message send callback
  , fgnCommand :: FunPtr ProcessCommand -- ^ Optional client command callback
  , fgnName    :: CString               -- ^ Null-terminated name
  , fgnMajorVersion, fgnMinorVersion :: CInt -- ^ extension version
  }

-- | @struct glirc_extension@
instance Storable FgnExtension where
  alignment _ = #alignment struct glirc_extension
  sizeOf    _ = #size      struct glirc_extension
  peek p      = FgnExtension
            <$> (#peek struct glirc_extension, start          ) p
            <*> (#peek struct glirc_extension, stop           ) p
            <*> (#peek struct glirc_extension, process_message) p
            <*> (#peek struct glirc_extension, process_chat   ) p
            <*> (#peek struct glirc_extension, process_command) p
            <*> (#peek struct glirc_extension, name           ) p
            <*> (#peek struct glirc_extension, major_version  ) p
            <*> (#peek struct glirc_extension, minor_version  ) p
  poke p FgnExtension{..} =
             do (#poke struct glirc_extension, start          ) p fgnStart
                (#poke struct glirc_extension, stop           ) p fgnStop
                (#poke struct glirc_extension, process_message) p fgnMessage
                (#poke struct glirc_extension, process_chat   ) p fgnChat
                (#poke struct glirc_extension, process_command) p fgnCommand
                (#poke struct glirc_extension, name           ) p fgnName
                (#poke struct glirc_extension, major_version  ) p fgnMajorVersion
                (#poke struct glirc_extension, minor_version  ) p fgnMinorVersion

------------------------------------------------------------------------

-- | @struct glirc_message@
data FgnMsg = FgnMsg
  { fmNetwork    :: FgnStringLen
  , fmPrefixNick :: FgnStringLen
  , fmPrefixUser :: FgnStringLen
  , fmPrefixHost :: FgnStringLen
  , fmCommand    :: FgnStringLen
  , fmParams     :: Ptr FgnStringLen -- ^ array
  , fmParamN     :: CSize            -- ^ array length
  , fmTagKeys    :: Ptr FgnStringLen -- ^ array
  , fmTagVals    :: Ptr FgnStringLen -- ^ array
  , fmTagN       :: CSize            -- ^ array length
  }

instance Storable FgnMsg where
  alignment _ = #alignment struct glirc_message
  sizeOf    _ = #size      struct glirc_message
  peek p      = FgnMsg
            <$> (#peek struct glirc_message, network ) p
            <*> (#peek struct glirc_message, prefix_nick) p
            <*> (#peek struct glirc_message, prefix_user) p
            <*> (#peek struct glirc_message, prefix_host) p
            <*> (#peek struct glirc_message, command ) p
            <*> (#peek struct glirc_message, params  ) p
            <*> (#peek struct glirc_message, params_n) p
            <*> (#peek struct glirc_message, tagkeys ) p
            <*> (#peek struct glirc_message, tagvals ) p
            <*> (#peek struct glirc_message, tags_n  ) p

  poke p FgnMsg{..} =
             do (#poke struct glirc_message, network ) p fmNetwork
                (#poke struct glirc_message, prefix_nick) p fmPrefixNick
                (#poke struct glirc_message, prefix_user) p fmPrefixUser
                (#poke struct glirc_message, prefix_host) p fmPrefixHost
                (#poke struct glirc_message, command ) p fmCommand
                (#poke struct glirc_message, params  ) p fmParams
                (#poke struct glirc_message, params_n) p fmParamN
                (#poke struct glirc_message, tagkeys ) p fmTagKeys
                (#poke struct glirc_message, tagvals ) p fmTagVals
                (#poke struct glirc_message, tags_n  ) p fmTagN

------------------------------------------------------------------------

-- | Chat message data containing the source network, window target,
-- and message body.
data FgnChat = FgnChat
  { fhNetwork    :: FgnStringLen
  , fhTarget     :: FgnStringLen
  , fhMessage    :: FgnStringLen
  }

-- | @struct glirc_message@
instance Storable FgnChat where
  alignment _ = #alignment struct glirc_chat
  sizeOf    _ = #size      struct glirc_chat
  peek p      = FgnChat
            <$> (#peek struct glirc_chat, network) p
            <*> (#peek struct glirc_chat, target ) p
            <*> (#peek struct glirc_chat, message) p

  poke p FgnChat{..} =
             do (#poke struct glirc_chat, network) p fhNetwork
                (#poke struct glirc_chat, target ) p fhTarget
                (#poke struct glirc_chat, message) p fhMessage

------------------------------------------------------------------------

-- | Used to pass arguments from @/extension EXT_NAME@ client command into
-- an extension.
data FgnCmd = FgnCmd
  { fcCommand :: FgnStringLen
  }

-- | @struct glirc_command@
instance Storable FgnCmd where
  alignment _ = #alignment struct glirc_command
  sizeOf    _ = #size      struct glirc_command
  peek p      = FgnCmd
            <$> (#peek struct glirc_command, command) p

  poke p FgnCmd{..} = (#poke struct glirc_command, command) p fcCommand

------------------------------------------------------------------------

-- | Pointer to UTF-8 encoded string and as string length. Strings are
-- null-terminated. The null-terminator is not counted in the length.
data FgnStringLen = FgnStringLen !CString !CSize

-- | @struct glirc_string@
instance Storable FgnStringLen where
  alignment _ = #alignment struct glirc_string
  sizeOf    _ = #size      struct glirc_string
  peek p      = FgnStringLen
            <$> (#peek struct glirc_string, str) p
            <*> (#peek struct glirc_string, len) p
  poke p (FgnStringLen x y) =
             do (#poke struct glirc_string, str) p x
                (#poke struct glirc_string, len) p y

------------------------------------------------------------------------

-- | Like 'poke' except it doesn't write to NULL
poke' :: Storable a => Ptr a -> a -> IO ()
poke' ptr x = unless (nullPtr == ptr) (poke ptr x)

-- | Marshal a text as a malloced null-terminated CStringLen
exportText :: Ptr CString -> Ptr CSize -> Text -> IO ()
exportText dstP dstL txt =

  Text.withCStringLen txt $ \(srcP, srcL) ->
    do poke' dstL (fromIntegral srcL)
       unless (dstP == nullPtr) $
         do a <- mallocArray0 srcL
            copyArray a srcP srcL
            pokeElemOff a srcL 0
            poke dstP a

-- | Marshal a text as a temporary null-terminated CStringLen
withText0 :: Text -> (CStringLen -> IO a) -> IO a
withText0 txt k =
  Text.withCStringLen txt $ \(p,l) ->
  allocaArray0 l $ \p' ->
    do copyArray p' p l
       pokeElemOff p' l 0
       k (p', l)
