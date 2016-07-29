{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Client.ServerSettings
Description : Settings for an individual IRC connection
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module defines the settings used for an individual IRC connection.
These are static settings that are not expected change over the lifetime
of a connection.
-}

module Client.ServerSettings
  (
  -- * Server settings type
    ServerSettings(..)
  , ssNick
  , ssUser
  , ssReal
  , ssUserInfo
  , ssPassword
  , ssSaslUsername
  , ssSaslPassword
  , ssHostName
  , ssPort
  , ssTls
  , ssTlsInsecure
  , ssTlsClientCert
  , ssTlsClientKey
  , ssConnectCmds
  , ssSocksHost
  , ssSocksPort
  , ssServerCerts
  , ssChanservChannels
  , ssFloodPenalty
  , ssFloodThreshold

  -- * Load function
  , loadDefaultServerSettings

  ) where

import           Control.Lens
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Irc.Identifier (Identifier)
import           System.Environment
import qualified Data.Text as Text

import Network.Socket (HostName, PortNumber)

-- | Static server-level settings
data ServerSettings = ServerSettings
  { _ssNick             :: !Text -- ^ connection nickname
  , _ssUser             :: !Text -- ^ connection username
  , _ssReal             :: !Text -- ^ connection realname / GECOS
  , _ssUserInfo         :: !Text -- ^ CTCP userinfo
  , _ssPassword         :: !(Maybe Text) -- ^ server password
  , _ssSaslUsername     :: !(Maybe Text) -- ^ SASL username
  , _ssSaslPassword     :: !(Maybe Text) -- ^ SASL password
  , _ssHostName         :: !HostName -- ^ server hostname
  , _ssPort             :: !(Maybe PortNumber) -- ^ server port
  , _ssTls              :: !Bool -- ^ use TLS to connect
  , _ssTlsInsecure      :: !Bool -- ^ disable certificate checking
  , _ssTlsClientCert    :: !(Maybe FilePath) -- ^ path to client TLS certificate
  , _ssTlsClientKey     :: !(Maybe FilePath) -- ^ path to client TLS key
  , _ssConnectCmds      :: ![Text] -- ^ raw IRC messages to transmit upon successful connection
  , _ssSocksHost        :: !(Maybe HostName) -- ^ hostname of SOCKS proxy
  , _ssSocksPort        :: !PortNumber -- ^ port of SOCKS proxy
  , _ssServerCerts      :: ![FilePath] -- ^ additional CA certificates for validating server
  , _ssChanservChannels :: ![Identifier] -- ^ Channels with chanserv permissions
  , _ssFloodPenalty     :: !Int -- ^ Flood limiter penalty (seconds)
  , _ssFloodThreshold   :: !Int -- ^ Flood limited threshold (seconds)
  }
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
       { _ssNick          = username
       , _ssUser          = username
       , _ssReal          = username
       , _ssUserInfo      = username
       , _ssPassword      = Text.pack <$> lookup "IRCPASSWORD" env
       , _ssSaslUsername  = Nothing
       , _ssSaslPassword  = Text.pack <$> lookup "SASLPASSWORD" env
       , _ssHostName      = ""
       , _ssPort          = Nothing
       , _ssTls           = False
       , _ssTlsInsecure   = False
       , _ssTlsClientCert = Nothing
       , _ssTlsClientKey  = Nothing
       , _ssConnectCmds   = []
       , _ssSocksHost     = Nothing
       , _ssSocksPort     = 1080
       , _ssServerCerts   = []
       , _ssChanservChannels = []
       , _ssFloodPenalty     = 2 -- RFC 1459 defaults
       , _ssFloodThreshold   = 10
       }
