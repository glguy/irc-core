{-# Language ForeignFunctionInterface, RecordWildCards #-}

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
  ( FgnExtension(..)
  , FgnStringLen(..)
  , FgnMsg(..)
  , FgnCmd(..)
  , runStartExtension
  , runStopExtension
  , runProcessMessage
  , runProcessCommand

  -- * report message codes
  , normalMessageCode
  , errorMessageCode
  ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable

normalMessageCode :: CInt
normalMessageCode = #const NORMAL_MESSAGE

errorMessageCode :: CInt
errorMessageCode = #const ERROR_MESSAGE

type StartExtension = Ptr () -> CString -> IO (Ptr ())
type StopExtension  = Ptr () -> Ptr () -> IO ()
type ProcessMessage = Ptr () -> Ptr () -> Ptr FgnMsg -> IO ()
type ProcessCommand = Ptr () -> Ptr () -> Ptr FgnCmd -> IO ()

type Dynamic a = FunPtr a -> a

foreign import ccall "dynamic" runStartExtension :: Dynamic StartExtension
foreign import ccall "dynamic" runStopExtension  :: Dynamic StopExtension
foreign import ccall "dynamic" runProcessMessage :: Dynamic ProcessMessage
foreign import ccall "dynamic" runProcessCommand :: Dynamic ProcessCommand

------------------------------------------------------------------------

data FgnExtension = FgnExtension
  { fgnStart   :: FunPtr StartExtension
  , fgnStop    :: FunPtr StopExtension
  , fgnMessage :: FunPtr ProcessMessage
  , fgnCommand :: FunPtr ProcessCommand
  , fgnName    :: CString
  , fgnMajorVersion, fgnMinorVersion :: CInt
  }

instance Storable FgnExtension where
  alignment _ = #alignment struct glirc_extension
  sizeOf    _ = #size      struct glirc_extension
  peek p      = FgnExtension
            <$> (#peek struct glirc_extension, start          ) p
            <*> (#peek struct glirc_extension, stop           ) p
            <*> (#peek struct glirc_extension, process_message) p
            <*> (#peek struct glirc_extension, process_command) p
            <*> (#peek struct glirc_extension, name           ) p
            <*> (#peek struct glirc_extension, major_version  ) p
            <*> (#peek struct glirc_extension, minor_version  ) p
  poke p FgnExtension{..} =
             do (#poke struct glirc_extension, start          ) p fgnStart
                (#poke struct glirc_extension, stop           ) p fgnStop
                (#poke struct glirc_extension, process_message) p fgnMessage
                (#poke struct glirc_extension, process_command) p fgnCommand
                (#poke struct glirc_extension, name           ) p fgnName
                (#poke struct glirc_extension, major_version  ) p fgnMajorVersion
                (#poke struct glirc_extension, minor_version  ) p fgnMinorVersion

------------------------------------------------------------------------

data FgnMsg = FgnMsg
  { fmNetwork :: FgnStringLen
  , fmPrefixNick :: FgnStringLen
  , fmPrefixUser :: FgnStringLen
  , fmPrefixHost :: FgnStringLen
  , fmCommand :: FgnStringLen
  , fmParams  :: Ptr FgnStringLen
  , fmParamN  :: CSize
  , fmTagKeys :: Ptr FgnStringLen
  , fmTagVals :: Ptr FgnStringLen
  , fmTagN    :: CSize
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

data FgnCmd = FgnCmd
  { fcParams  :: Ptr FgnStringLen
  , fcParamN  :: CSize
  }

instance Storable FgnCmd where
  alignment _ = #alignment struct glirc_command
  sizeOf    _ = #size      struct glirc_command
  peek p      = FgnCmd
            <$> (#peek struct glirc_command, params  ) p
            <*> (#peek struct glirc_command, params_n) p

  poke p FgnCmd{..} =
             do (#poke struct glirc_command, params  ) p fcParams
                (#poke struct glirc_command, params_n) p fcParamN

------------------------------------------------------------------------

data FgnStringLen = FgnStringLen !CString !CSize

instance Storable FgnStringLen where
  alignment _ = #alignment struct glirc_string
  sizeOf    _ = #size      struct glirc_string
  peek p      = FgnStringLen
            <$> (#peek struct glirc_string, str) p
            <*> (#peek struct glirc_string, len) p
  poke p (FgnStringLen x y) =
             do (#poke struct glirc_string, str) p x
                (#poke struct glirc_string, len) p y
