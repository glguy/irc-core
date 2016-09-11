{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Client.Configuration.ServerSettings
Description : Settings for an individual IRC connection
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module defines the settings used for an individual IRC connection.
These are static settings that are not expected change over the lifetime
of a connection.
-}

module Client.Configuration.ServerSettings
  (
  -- * Server settings type
    ServerSettings(..)

  -- * Lenses
  , ssNicks
  , ssUser
  , ssReal
  , ssUserInfo
  , ssPassword
  , ssSaslUsername
  , ssSaslPassword
  , ssHostName
  , ssPort
  , ssTls
  , ssTlsClientCert
  , ssTlsClientKey
  , ssConnectCmds
  , ssSocksHost
  , ssSocksPort
  , ssServerCerts
  , ssChanservChannels
  , ssFloodPenalty
  , ssFloodThreshold
  , ssMessageHooks
  , ssName
  , ssReconnectAttempts

  -- * Load function
  , loadDefaultServerSettings

  -- * TLS settings
  , UseTls(..)

  ) where

import           Client.Commands.Interpolation
import           Control.Lens
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Irc.Identifier (Identifier)
import           Network.Socket (HostName, PortNumber)
import           System.Environment

-- | Static server-level settings
data ServerSettings = ServerSettings
  { _ssNicks            :: !(NonEmpty Text) -- ^ connection nicknames
  , _ssUser             :: !Text -- ^ connection username
  , _ssReal             :: !Text -- ^ connection realname / GECOS
  , _ssUserInfo         :: !Text -- ^ CTCP userinfo
  , _ssPassword         :: !(Maybe Text) -- ^ server password
  , _ssSaslUsername     :: !(Maybe Text) -- ^ SASL username
  , _ssSaslPassword     :: !(Maybe Text) -- ^ SASL password
  , _ssHostName         :: !HostName -- ^ server hostname
  , _ssPort             :: !(Maybe PortNumber) -- ^ server port
  , _ssTls              :: !UseTls -- ^ use TLS to connect
  , _ssTlsClientCert    :: !(Maybe FilePath) -- ^ path to client TLS certificate
  , _ssTlsClientKey     :: !(Maybe FilePath) -- ^ path to client TLS key
  , _ssConnectCmds      :: ![[ExpansionChunk]] -- ^ commands to execute upon successful connection
  , _ssSocksHost        :: !(Maybe HostName) -- ^ hostname of SOCKS proxy
  , _ssSocksPort        :: !PortNumber -- ^ port of SOCKS proxy
  , _ssServerCerts      :: ![FilePath] -- ^ additional CA certificates for validating server
  , _ssChanservChannels :: ![Identifier] -- ^ Channels with chanserv permissions
  , _ssFloodPenalty     :: !Rational -- ^ Flood limiter penalty (seconds)
  , _ssFloodThreshold   :: !Rational -- ^ Flood limited threshold (seconds)
  , _ssMessageHooks     :: ![Text] -- ^ Initial message hooks
  , _ssName             :: !(Maybe Text) -- ^ The name referencing the server in commands
  , _ssReconnectAttempts:: !Int -- ^ The number of reconnect attempts to make on error
  }
  deriving Show

data UseTls
  = UseTls         -- ^ TLS connection
  | UseInsecureTls -- ^ TLS connection without certificate checking
  | UseInsecure    -- ^ Plain connection
  deriving Show

makeLenses ''ServerSettings

-- | Load the defaults for server settings based on the environment
-- variables.
--
-- @USER@, @IRCPASSSWORD@, and @SASLPASSWORD@ are used.
loadDefaultServerSettings :: IO ServerSettings
loadDefaultServerSettings =
  do env  <- getEnvironment
     let username = Text.pack (fromMaybe "guest" (lookup "USER" env))
     return ServerSettings
       { _ssNicks         = username NonEmpty.:| []
       , _ssUser          = username
       , _ssReal          = username
       , _ssUserInfo      = username
       , _ssPassword      = Text.pack <$> lookup "IRCPASSWORD" env
       , _ssSaslUsername  = Nothing
       , _ssSaslPassword  = Text.pack <$> lookup "SASLPASSWORD" env
       , _ssHostName      = ""
       , _ssPort          = Nothing
       , _ssTls           = UseInsecure
       , _ssTlsClientCert = Nothing
       , _ssTlsClientKey  = Nothing
       , _ssConnectCmds   = []
       , _ssSocksHost     = Nothing
       , _ssSocksPort     = 1080
       , _ssServerCerts   = []
       , _ssChanservChannels = []
       , _ssFloodPenalty     = 2 -- RFC 1459 defaults
       , _ssFloodThreshold   = 10
       , _ssMessageHooks     = []
       , _ssName             = Nothing
       , _ssReconnectAttempts= 6 -- six feels great
       }
