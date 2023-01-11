{-# Language OverloadedStrings, PatternSynonyms #-}
{-|
Module      : Client.EventLoop.Network
Description : Event handlers for network messages affecting the client state
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

While most network messages only affect the model of that network connection,
some messages will affect the mutable state of the client itself.
-}
module Client.EventLoop.Network
  ( clientResponse
  ) where

import Client.Authentication.Ecdsa qualified as Ecdsa
import Client.Commands (CommandResult(CommandQuit, CommandFailure, CommandSuccess), commandExpansion, executeUserCommand)
import Client.Commands.Interpolation (resolveMacroExpansions, ExpansionChunk)
import Client.Configuration.ServerSettings
import Client.Configuration.Sts (savePolicyFile, StsPolicy(StsPolicy, _stsPort, _stsExpiration))
import Client.Network.Async (abortConnection, TerminationReason(StsUpgrade))
import Client.Network.Connect (ircPort)
import Client.State
import Client.State.Focus (Focus(ChannelFocus, NetworkFocus))
import Client.State.Network
import Control.Lens (view, (&), folded, previews, views, (?~), set, At(at))
import Control.Monad (join, foldM, forM)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text
import Data.Time (ZonedTime, addUTCTime, getCurrentTime, formatTime, defaultTimeLocale, utcToLocalZonedTime)
import Irc.Codes (pattern ERR_LINKCHANNEL, pattern RPL_WELCOME)
import Irc.Commands (ircAuthenticate, ircCapEnd)
import Irc.Identifier (mkId)
import Irc.Message (IrcMsg(Error, Reply, Authenticate, Cap), CapCmd(CapNew, CapLs))
import Text.Regex.TDFA.Text qualified as Regex

-- | Client-level responses to specific IRC messages.
-- This is in contrast to the connection state tracking logic in
-- "Client.NetworkState"
clientResponse :: ZonedTime -> IrcMsg -> NetworkState -> ClientState -> IO ClientState
clientResponse now irc cs st =
  case irc of
    Reply _ RPL_WELCOME _ ->
      -- run connection commands with the network focused and restore it afterward
      do let focus = NetworkFocus (view csNetwork cs)
         st' <- foldM (processConnectCmd now cs)
                      (set clientFocus focus st)
                      (view (csSettings . ssConnectCmds) cs)
         return $! set clientFocus (view clientFocus st) st'

    -- Change focus when we get a message that we're being forwarded to another channel
    Reply _ ERR_LINKCHANNEL (_ : src : dst : _)
      | let network = view csNetwork cs
      , view clientFocus st == ChannelFocus network (mkId src) ->
         return $! set clientFocus (ChannelFocus network (mkId dst)) st

    Authenticate challenge
      | AS_EcdsaWaitChallenge <- view csAuthenticationState cs ->
         processSaslEcdsa now challenge cs st

    Cap (CapLs _ caps)
      | Just stsVal <- join (lookup "sts" caps) -> processSts stsVal cs st

    Cap (CapNew caps)
      | Just stsVal <- join (lookup "sts" caps) -> processSts stsVal cs st

    Error msg
      | Just rx <- previews (csSettings . ssReconnectError . folded) getRegex cs
      , Right{} <- Regex.execute rx msg
      , let discoTime = view csLastReceived cs ->
         addConnection 1 discoTime Nothing (view csNetwork cs) st

    _ -> return st

processSts ::
  Text         {- ^ STS parameter string -} ->
  NetworkState {- ^ network state        -} ->
  ClientState  {- ^ client state         -} ->
  IO ClientState
processSts txt cs st =
  case view (csSettings . ssTls) cs of
    _      | views (csSettings . ssSts) not cs -> return st -- sts disabled
    TlsNo  | Just port     <- mbPort           -> upgradeConnection port
    TlsYes | Just duration <- mbDuration       -> setStsPolicy duration
    _                                          -> return st

  where
    entries    = splitEntry <$> Text.splitOn "," txt
    mbPort     = readInt =<< lookup "port"     entries
    mbDuration = readInt =<< lookup "duration" entries

    splitEntry e =
      case Text.break ('=' ==) e of
        (a, b) -> (a, Text.drop 1 b)

    upgradeConnection port =
      do abortConnection StsUpgrade (view csSocket cs)
         addConnection 0 (view csLastReceived cs) (Just port) (view csNetwork cs) st

    setStsPolicy duration =
      do now <- getCurrentTime
         let host = Text.pack (view (csSettings . ssHostName) cs)
             port = fromIntegral (ircPort (view csSettings cs))
             policy = StsPolicy
                        { _stsExpiration = addUTCTime (fromIntegral duration) now
                        , _stsPort       = port }
             st' = st & clientStsPolicy . at host ?~ policy
         savePolicyFile (view clientStsPolicy st')
         return st'


readInt :: Text -> Maybe Int
readInt x =
  case Text.decimal x of
    Right (n, t) | Text.null t -> Just n
    _                          -> Nothing

processSaslEcdsa ::
  ZonedTime    {- ^ message time  -} ->
  Text         {- ^ challenge     -} ->
  NetworkState {- ^ network state -} ->
  ClientState  {- ^ client state  -} ->
  IO ClientState
processSaslEcdsa now challenge cs st =
  case view ssSaslMechanism ss of
    Just (SaslEcdsa _ _ path) ->
      do res <- Ecdsa.computeResponse path challenge
         case res of
           Left e ->
             do sendMsg cs ircCapEnd
                return $! recordError now (view csNetwork cs) (Text.pack e) st
           Right resp ->
             do sendMsg cs (ircAuthenticate resp)
                return $! set asLens AS_None st

    _ ->
      do sendMsg cs ircCapEnd
         return $! recordError now (view csNetwork cs) "panic: ecdsa mechanism not configured" st
  where
    ss = view csSettings cs
    asLens = clientConnection (view csNetwork cs) . csAuthenticationState

processConnectCmd ::
  ZonedTime       {- ^ now             -} ->
  NetworkState    {- ^ current network -} ->
  ClientState     {- ^ client state    -} ->
  [ExpansionChunk]{- ^ command         -} ->
  IO ClientState
processConnectCmd now cs st0 cmdTxt =
  do dc <- forM disco $ \t ->
             Text.pack . formatTime defaultTimeLocale "%H:%M:%S"
               <$> utcToLocalZonedTime t
     let failureCase e = recordError now (view csNetwork cs) ("Bad connect-cmd: " <> e)
     case resolveMacroExpansions (commandExpansion dc st0) (const Nothing) cmdTxt of
       Nothing -> return $! failureCase "Unable to expand connect command" st0
       Just cmdTxt' ->
         do res <- executeUserCommand dc (Text.unpack cmdTxt') st0
            return $! case res of
              CommandFailure st -> failureCase cmdTxt' st
              CommandSuccess st -> st
              CommandQuit    st -> st -- not supported
 where
 disco =
   case view csPingStatus cs of
     PingConnecting _ tm _ -> tm
     _                     -> Nothing
