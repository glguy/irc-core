{-# Language ForeignFunctionInterface #-}

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
  , runStartExtension
  , runStopExtension
  , runProcessMessage
  ) where

import Control.Applicative
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

type StartExtension    = IO (Ptr ())
type StopExtension     = Ptr () -> IO ()
type ProcessMessage = Ptr () -> Ptr FgnMsg -> IO ()

type Dynamic a = FunPtr a -> a

foreign import ccall "dynamic" runStartExtension :: Dynamic StartExtension
foreign import ccall "dynamic" runStopExtension  :: Dynamic StopExtension
foreign import ccall "dynamic" runProcessMessage :: Dynamic ProcessMessage

------------------------------------------------------------------------

data FgnExtension = FgnExtension
  { fgnStart   :: FunPtr StartExtension
  , fgnStop    :: FunPtr StopExtension
  , fgnProcess :: FunPtr ProcessMessage
  }

instance Storable FgnExtension where
  alignment _ = #alignment struct glirc_extension
  sizeOf    _ = #size      struct glirc_extension
  peek p      = FgnExtension
            <$> (#peek struct glirc_extension, start   ) p
            <*> (#peek struct glirc_extension, stop    ) p
            <*> (#peek struct glirc_extension, process_message) p
  poke p (FgnExtension x y z) =
     do (#poke struct glirc_extension, start   ) p x
        (#poke struct glirc_extension, stop    ) p y
        (#poke struct glirc_extension, process_message) p z

------------------------------------------------------------------------

data FgnMsg = FgnMsg
  { fgnNetwork :: FgnStringLen
  , fgnPrefix  :: FgnStringLen
  , fgnCommand :: FgnStringLen
  , fgnParams  :: Ptr FgnStringLen
  , fgnParamN  :: CSize
  }

instance Storable FgnMsg where
  alignment _ = #alignment struct glirc_message
  sizeOf    _ = #size      struct glirc_message
  peek p      = FgnMsg
            <$> peek ((#ptr struct glirc_message, network) p)
            <*> peek ((#ptr struct glirc_message, command) p)
            <*> peek ((#ptr struct glirc_message, prefix ) p)
            <*> (#peek struct glirc_message, params ) p
            <*> (#peek struct glirc_message, params_n) p

  poke p (FgnMsg w x y z zn) =
     do poke ((#ptr struct glirc_message, network) p) w
        poke ((#ptr struct glirc_message, prefix) p) y
        poke ((#ptr struct glirc_message, command) p) y
        (#poke struct glirc_message, params) p z
        (#poke struct glirc_message, params_n) p zn

data FgnStringLen = FgnStringLen CString CSize

instance Storable FgnStringLen where
  alignment _ = #alignment struct glirc_string
  sizeOf    _ = #size      struct glirc_string
  peek p      = FgnStringLen
            <$> (#peek struct glirc_string, str) p
            <*> (#peek struct glirc_string, len) p
  poke p (FgnStringLen x y) =
             do (#poke struct glirc_string, str) p x
                (#poke struct glirc_string, len) p y
