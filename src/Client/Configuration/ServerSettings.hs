{-# LANGUAGE ApplicativeDo, TemplateHaskell, OverloadedStrings #-}

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
  , serverSpec
  , identifierSpec

  -- * Lenses
  , ssNicks
  , ssUser
  , ssReal
  , ssUserInfo
  , ssPassword
  , ssSaslUsername
  , ssSaslPassword
  , ssSaslEcdsaFile
  , ssHostName
  , ssPort
  , ssTls
  , ssTlsClientCert
  , ssTlsClientKey
  , ssTlsServerCert
  , ssTlsCiphers
  , ssConnectCmds
  , ssSocksHost
  , ssSocksPort
  , ssChanservChannels
  , ssFloodPenalty
  , ssFloodThreshold
  , ssMessageHooks
  , ssName
  , ssReconnectAttempts
  , ssAutoconnect
  , ssNickCompletion
  , ssLogDir

  -- * Load function
  , loadDefaultServerSettings

  -- * TLS settings
  , UseTls(..)

  ) where

import           Client.Commands.Interpolation
import           Client.Commands.WordCompletion
import           Client.Configuration.Macros (macroCommandSpec)
import           Config.Schema.Spec
import           Control.Lens
import           Data.Functor.Alt                    ((<!>))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import           Irc.Identifier (Identifier, mkId)
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
  , _ssSaslPassword     :: !(Maybe Text) -- ^ SASL plain password
  , _ssSaslEcdsaFile    :: !(Maybe FilePath) -- ^ SASL ecdsa private key
  , _ssHostName         :: !HostName -- ^ server hostname
  , _ssPort             :: !(Maybe PortNumber) -- ^ server port
  , _ssTls              :: !UseTls -- ^ use TLS to connect
  , _ssTlsClientCert    :: !(Maybe FilePath) -- ^ path to client TLS certificate
  , _ssTlsClientKey     :: !(Maybe FilePath) -- ^ path to client TLS key
  , _ssTlsServerCert    :: !(Maybe FilePath) -- ^ additional CA certificates for validating server
  , _ssTlsCiphers       :: String            -- ^ OpenSSL cipher suite
  , _ssConnectCmds      :: ![[ExpansionChunk]] -- ^ commands to execute upon successful connection
  , _ssSocksHost        :: !(Maybe HostName) -- ^ hostname of SOCKS proxy
  , _ssSocksPort        :: !PortNumber -- ^ port of SOCKS proxy
  , _ssChanservChannels :: ![Identifier] -- ^ Channels with chanserv permissions
  , _ssFloodPenalty     :: !Rational -- ^ Flood limiter penalty (seconds)
  , _ssFloodThreshold   :: !Rational -- ^ Flood limited threshold (seconds)
  , _ssMessageHooks     :: ![Text] -- ^ Initial message hooks
  , _ssName             :: !(Maybe Text) -- ^ The name referencing the server in commands
  , _ssReconnectAttempts:: !Int -- ^ The number of reconnect attempts to make on error
  , _ssAutoconnect      :: Bool -- ^ Connect to this network on server startup
  , _ssNickCompletion   :: WordCompletionMode -- ^ Nick completion mode for this server
  , _ssLogDir           :: Maybe FilePath -- ^ Directory to save logs of chat
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
       , _ssSaslEcdsaFile = Nothing
       , _ssHostName      = ""
       , _ssPort          = Nothing
       , _ssTls           = UseInsecure
       , _ssTlsClientCert = Nothing
       , _ssTlsClientKey  = Nothing
       , _ssTlsServerCert = Nothing
       , _ssTlsCiphers    = "HIGH"
       , _ssConnectCmds   = []
       , _ssSocksHost     = Nothing
       , _ssSocksPort     = 1080
       , _ssChanservChannels = []
       , _ssFloodPenalty     = 2 -- RFC 1459 defaults
       , _ssFloodThreshold   = 10
       , _ssMessageHooks     = []
       , _ssName             = Nothing
       , _ssReconnectAttempts= 6 -- six feels great
       , _ssAutoconnect      = False
       , _ssNickCompletion   = defaultNickWordCompleteMode
       , _ssLogDir           = Nothing
       }

serverSpec :: ValueSpecs (ServerSettings -> ServerSettings)
serverSpec = sectionsSpec "server-settings" $
  ala Endo (foldMap . foldMap) <$> sequenceA settings
  where
    req l s = set l <$> s

    opt l s = set l . Just <$> s
          <!> set l Nothing <$ atomSpec "clear"

    settings =
      [ optSection' "name" "The name used to identify this server in the client"
      $ opt ssName valuesSpec
      , optSection' "hostname" "Hostname of server"
      $ req ssHostName stringSpec
      , optSection' "port" "Port number of server. Default 6667 without TLS or 6697 with TLS"
      $ opt ssPort numSpec
      , optSection' "nick" "Nicknames to connect with in order"
      $ req ssNicks nicksSpec
      , optSection' "password" "Server password"
      $ opt ssPassword valuesSpec
      , optSection' "username" "Second component of _!_@_ usermask"
      $ req ssUser valuesSpec
      , optSection' "realname" "\"GECOS\" name sent to server visible in /whois"
      $ req ssReal valuesSpec
      , optSection' "userinfo" "CTCP userinfo (currently unused)"
      $ req ssUserInfo valuesSpec
      , optSection' "sasl-username" "Username for SASL authentication to NickServ"
      $ opt ssSaslUsername valuesSpec
      , optSection' "sasl-password" "Password for SASL authentication to NickServ"
      $ opt ssSaslPassword valuesSpec
      , optSection' "sasl-ecdsa-key" "Path to ECDSA key for non-password SASL authentication"
      $ opt ssSaslEcdsaFile stringSpec
      , optSection' "tls" "Set to `yes` to enable secure connect. Set to `yes-insecure` to disable certificate checking."
      $ req ssTls useTlsSpec
      , optSection' "tls-client-cert" "Path to TLS client certificate"
      $ opt ssTlsClientCert stringSpec
      , optSection' "tls-client-key" "Path to TLS client key"
      $ opt ssTlsClientKey stringSpec
      , optSection' "tls-server-cert" "Path to CA certificate bundle"
      $ opt ssTlsServerCert stringSpec
      , optSection' "tls-ciphers" "OpenSSL cipher specification. Default to \"HIGH\""
      $ req ssTlsCiphers stringSpec
      , optSection' "socks-host" "Hostname of SOCKS5 proxy server"
      $ opt ssSocksHost stringSpec
      , optSection' "socks-port" "Port number of SOCKS5 proxy server"
      $ req ssSocksPort numSpec
      , optSection' "connect-cmds" "Command to be run upon successful connection to server"
      $ req ssConnectCmds $ listSpec macroCommandSpec
      , optSection' "chanserv-channels" "Channels with ChanServ permissions available"
      $ req ssChanservChannels $ listSpec identifierSpec
      , optSection' "flood-penalty" "RFC 1459 rate limiting, seconds of penalty per message (default 2)"
      $ req ssFloodPenalty valuesSpec
      , optSection' "flood-threshold" "RFC 1459 rate limiting, seconds of allowed penalty accumulation (default 10)"
      $ req ssFloodThreshold valuesSpec
      , optSection' "message-hooks" "Special message hooks to enable: \"buffextras\" available"
      $ req ssMessageHooks valuesSpec
      , optSection' "reconnect-attempts" "Number of reconnection attempts on lost connection"
      $ req ssReconnectAttempts valuesSpec
      , optSection' "autoconnect" "Set to `yes` to automatically connect at client startup"
      $ req ssAutoconnect yesOrNoSpec
      , optSection' "nick-completion" "Behavior for nickname completion with TAB"
      $ req ssNickCompletion nickCompletionSpec
      , optSection' "log-dir" "Path to log file directory for this server"
      $ opt ssLogDir stringSpec
      ]


nicksSpec :: ValueSpecs (NonEmpty Text)
nicksSpec = oneOrNonemptySpec valuesSpec


useTlsSpec :: ValueSpecs UseTls
useTlsSpec =
      UseTls         <$ atomSpec "yes"
  <!> UseInsecureTls <$ atomSpec "yes-insecure"
  <!> UseInsecure    <$ atomSpec "no"


nickCompletionSpec :: ValueSpecs WordCompletionMode
nickCompletionSpec =
      defaultNickWordCompleteMode <$ atomSpec "default"
  <!> slackNickWordCompleteMode   <$ atomSpec "slack"


identifierSpec :: ValueSpecs Identifier
identifierSpec = mkId <$> valuesSpec
