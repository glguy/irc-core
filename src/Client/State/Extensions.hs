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
module Client.State.Extensions
  ( clientChatExtension
  , clientCommandExtension
  , clientStartExtensions
  , clientNotifyExtensions
  , clientStopExtensions
  ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Text (Text)
import Data.Time
import Foreign.Ptr
import Foreign.StablePtr
import qualified Data.Text as Text
import qualified Data.IntMap as IntMap

import Irc.RawIrcMsg

import Client.State
import Client.Message
import Client.CApi
import Client.CApi.Types
import Client.Configuration

-- | Start extensions after ensuring existing ones are stopped
clientStartExtensions ::
  ClientState    {- ^ client state                     -} ->
  IO ClientState {- ^ client state with new extensions -}
clientStartExtensions st =
  do let cfg = view clientConfig st
     st1 <- clientStopExtensions st
     foldM start1 st1 (view configExtensions cfg)

-- | Start a single extension and register it with the client or
-- record the error message.
start1 :: ClientState -> ExtensionConfiguration -> IO ClientState
start1 st config =
  do res <- try (openExtension config) :: IO (Either IOError ActiveExtension)

     case res of
       Left err ->
         do now <- getZonedTime
            return $! recordNetworkMessage ClientMessage
              { _msgTime    = now
              , _msgBody    = ErrorBody (Text.pack (displayException err))
              , _msgNetwork = ""
              } st

       Right ae ->
            -- allocate a new identity for this extension
         do let i = case IntMap.maxViewWithKey (view (clientExtensions . esActive) st) of
                      Just ((k,_),_) -> k+1
                      Nothing -> 0

            let st1 = st & clientExtensions . esActive . at i ?~ ae
            (st2, h) <- clientPark st1 $ \ptr -> startExtension ptr config ae

            -- save handle back into active extension
            return $! st2 & clientExtensions . esActive . ix i %~ \ae' ->
                        ae' { aeSession = h }



-- | Unload all active extensions.
clientStopExtensions ::
  ClientState    {- ^ client state                          -} ->
  IO ClientState {- ^ client state with extensions unloaded -}
clientStopExtensions st =
  do let (aes,st1) = (clientExtensions . esActive <<.~ IntMap.empty) st
     (st2,_) <- clientPark st1 $ \ptr ->
                  traverse_ (deactivateExtension ptr) aes
     return st2


-- | Dispatch chat messages through extensions before sending to server.
clientChatExtension ::
  Text        {- ^ network                     -} ->
  Text        {- ^ target                      -} ->
  Text        {- ^ message                     -} ->
  ClientState {- ^ client state, allow message -} ->
  IO (ClientState, Bool)
clientChatExtension net tgt msg st
  | noCallback = return (st, True)
  | otherwise  = evalNestedIO $
                 do chat <- withChat net tgt msg
                    liftIO (chat1 chat st aes)
  where
    aes = IntMap.elems (view (clientExtensions . esActive) st)
    noCallback = all (\ae -> fgnChat (aeFgn ae) == nullFunPtr) aes

chat1 ::
  Ptr FgnChat            {- ^ serialized chat message     -} ->
  ClientState            {- ^ client state                -} ->
  [ActiveExtension]      {- ^ extensions needing callback -} ->
  IO (ClientState, Bool) {- ^ new state and allow         -}
chat1 _    st [] = return (st, True)
chat1 chat st (ae:aes) =
  do (st1, allow) <- clientPark st $ \ptr -> chatExtension ptr ae chat
     if allow then chat1 chat st1 aes
              else return (st1, False)


-- | Dispatch incoming IRC message through extensions
clientNotifyExtensions ::
  Text                   {- ^ network                 -} ->
  RawIrcMsg              {- ^ incoming message        -} ->
  ClientState            {- ^ client state            -} ->
  IO (ClientState, Bool) {- ^ drop message when false -}
clientNotifyExtensions network raw st
  | noCallback = return (st, True)
  | otherwise  = evalNestedIO $
                 do fgn <- withRawIrcMsg network raw
                    liftIO (message1 fgn st aes)
  where
    aes = IntMap.elems (view (clientExtensions . esActive) st)
    noCallback = all (\ae -> fgnMessage (aeFgn ae) == nullFunPtr) aes

message1 ::
  Ptr FgnMsg             {- ^ serialized IRC message      -} ->
  ClientState            {- ^ client state                -} ->
  [ActiveExtension]      {- ^ extensions needing callback -} ->
  IO (ClientState, Bool) {- ^ new state and allow         -}
message1 _    st [] = return (st, True)
message1 chat st (ae:aes) =
  do (st1, allow) <- clientPark st $ \ptr -> notifyExtension ptr ae chat
     if allow then message1 chat st1 aes
              else return (st1, False)


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
