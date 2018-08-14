{-# Language OverloadedStrings #-}
{-|
Module      : Client.State.Extensions
Description : Integration between the client and external extensions
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

This module implements the interaction between the client and its extensions.
This includes aspects of the extension system that depend on the current client
state.
-}
module Client.State.Extensions where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Lens
import Control.Concurrent.MVar
import Control.Exception
import Foreign.Ptr
import Foreign.StablePtr

import Irc.RawIrcMsg

import Client.State
import Client.Message
import Client.CApi
import Client.Configuration
import Data.Either
import Data.Time
import Data.Foldable

-- | Start extensions after ensuring existing ones are stopped
clientStartExtensions ::
  ClientState    {- ^ client state                     -} ->
  IO ClientState {- ^ client state with new extensions -}
clientStartExtensions st =
  do let cfg = view clientConfig st
     st1        <- clientStopExtensions st
     (st2, res) <- clientPark st1 $ \ptr ->
            traverse (try . activateExtension ptr)
                     (view configExtensions cfg)

     let (errors, exts) = partitionEithers res
     st3 <- recordErrors errors st2
     return $! set (clientExtensions . esActive) exts st3
  where
    activateExtension ptr config =
      do ae <- openExtension config
         h  <- startExtension ptr config ae
         return ae { aeSession = h }

    recordErrors [] ste = return ste
    recordErrors es ste =
      do now <- getZonedTime
         return $! foldl' (recordError now) ste es

    recordError now ste e =
      recordNetworkMessage ClientMessage
        { _msgTime    = now
        , _msgBody    = ErrorBody (Text.pack (show (e :: IOError)))
        , _msgNetwork = ""
        } ste


-- | Unload all active extensions.
clientStopExtensions ::
  ClientState    {- ^ client state                          -} ->
  IO ClientState {- ^ client state with extensions unloaded -}
clientStopExtensions st =
  do let (aes,st1) = (clientExtensions . esActive <<.~ []) st
     (st2,_) <- clientPark st1 $ \ptr ->
                  traverse_ (deactivateExtension ptr) aes
     return st2


-- | Dispatch chat messages through extensions before sending to server.
clientChatExtension ::
  Text        {- ^ network      -} ->
  Text        {- ^ target       -} ->
  Text        {- ^ message      -} ->
  ClientState {- ^ client state -} ->
  IO (ClientState, Bool)
clientChatExtension network tgtTxt msgTxt st =
  clientPark st $ \ptr ->
    do let aes = view (clientExtensions . esActive) st
       chatExtension ptr network tgtTxt msgTxt aes


-- | Dispatch incoming IRC message through extensions
clientNotifyExtensions ::
  Text                   {- ^ network                 -} ->
  RawIrcMsg              {- ^ incoming message        -} ->
  ClientState            {- ^ client state            -} ->
  IO (ClientState, Bool) {- ^ drop message when false -}
clientNotifyExtensions network raw st =
  clientPark st $ \ptr ->
    notifyExtensions ptr network raw
      (view (clientExtensions . esActive) st)


-- | Dispatch @/extension@ command to correct extension. Returns
-- 'Nothing' when no matching extension is available.
clientCommandExtension ::
  Text                   {- ^ extension name              -} ->
  Text                   {- ^ command                     -} ->
  ClientState            {- ^ client state                -} ->
  IO (Maybe ClientState) {- ^ new client state on success -}
clientCommandExtension name command st =
  case find (\ae -> aeName ae == name)
            (view (clientExtensions . esActive) st) of
        Nothing -> return Nothing
        Just ae ->
          do (st', _) <- clientPark st $ \ptr ->
                            commandExtension ptr command ae
             return (Just st')


-- | Prepare the client to support reentry from the extension API.
clientPark ::
  ClientState      {- ^ client state                                        -} ->
  (Ptr () -> IO a) {- ^ continuation using the stable pointer to the client -} ->
  IO (ClientState, a)
clientPark st k =
  do let mvar = view (clientExtensions . esMVar) st
     putMVar mvar st
     let token = views (clientExtensions . esStablePtr) castStablePtrToPtr st
     res <- k token
     st' <- takeMVar mvar
     return (st', res)
