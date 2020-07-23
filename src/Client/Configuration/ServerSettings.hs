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
  , HookConfig(..)
  , serverSpec
  , identifierSpec

  -- * Lenses
  , ssNicks
  , ssUser
  , ssReal
  , ssPassword
  , ssSaslMechanism
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
  , ssReconnectError
  , ssAutoconnect
  , ssNickCompletion
  , ssLogDir
  , ssProtocolFamily
  , ssSts
  , ssTlsPubkeyFingerprint
  , ssTlsCertFingerprint
  , ssShowAccounts
  , ssCapabilities

  -- * SASL Mechanisms
  , SaslMechanism(..)
  , _SaslExternal
  , _SaslEcdsa
  , _SaslPlain

  -- * Defaults
  , defaultServerSettings

  -- * TLS settings
  , UseTls(..)
  , Fingerprint(..)

  -- * Regex wrapper
  , KnownRegex(..)
  , getRegex
  ) where

import           Client.Commands.Interpolation
import           Client.Commands.WordCompletion
import           Client.Configuration.Macros (macroCommandSpec)
import           Config.Schema.Spec
import           Control.Lens
import qualified Data.ByteString as B
import           Data.Functor.Alt                    ((<!>))
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Text (Text)
import           Data.List.Split (chunksOf, splitOn)
import qualified Data.Text as Text
import           Irc.Identifier (Identifier, mkId)
import           Network.Socket (HostName, PortNumber, Family(..))
import           Numeric (readHex)
import           Text.Regex.TDFA
import           Text.Regex.TDFA.Text (compile)

-- | Static server-level settings
data ServerSettings = ServerSettings
  { _ssNicks            :: !(NonEmpty Text) -- ^ connection nicknames
  , _ssUser             :: !Text -- ^ connection username
  , _ssReal             :: !Text -- ^ connection realname / GECOS
  , _ssPassword         :: !(Maybe Text) -- ^ server password
  , _ssSaslMechanism    :: !(Maybe SaslMechanism) -- ^ SASL mechanism
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
  , _ssMessageHooks     :: ![HookConfig] -- ^ Initial message hooks
  , _ssName             :: !(Maybe Text) -- ^ The name referencing the server in commands
  , _ssReconnectAttempts:: !Int -- ^ The number of reconnect attempts to make on error
  , _ssReconnectError   :: !(Maybe KnownRegex) -- ^ Regular expression for ERROR messages that trigger reconnect
  , _ssAutoconnect      :: !Bool -- ^ Connect to this network on server startup
  , _ssNickCompletion   :: WordCompletionMode -- ^ Nick completion mode for this server
  , _ssLogDir           :: Maybe FilePath -- ^ Directory to save logs of chat
  , _ssProtocolFamily   :: Maybe Family -- ^ Protocol family to connect with
  , _ssSts              :: !Bool -- ^ Honor STS policies when true
  , _ssTlsPubkeyFingerprint :: !(Maybe Fingerprint) -- ^ optional acceptable public key fingerprint
  , _ssTlsCertFingerprint   :: !(Maybe Fingerprint) -- ^ optional acceptable certificate fingerprint
  , _ssShowAccounts     :: !Bool -- ^ Render account names
  , _ssCapabilities     :: ![Text] -- ^ Extra capabilities to unconditionally request
  }
  deriving Show

-- | SASL mechanisms and configuration data.
data SaslMechanism
  = SaslPlain    (Maybe Text) Text Text -- ^ SASL PLAIN RFC4616 - authzid authcid password
  | SaslEcdsa    (Maybe Text) Text FilePath -- ^ SASL NIST - https://github.com/kaniini/ecdsatool - authzid keypath
  | SaslExternal (Maybe Text)      -- ^ SASL EXTERNAL RFC4422 - authzid
  deriving Show

-- | Regular expression matched with original source to help with debugging.
data KnownRegex = KnownRegex Text Regex

getRegex :: KnownRegex -> Regex
getRegex (KnownRegex _ r) = r

instance Show KnownRegex where show (KnownRegex x _) = show x

-- | Hook name and configuration arguments
data HookConfig = HookConfig Text [Text]
  deriving Show

-- | Security setting for network connection
data UseTls
  = UseTls         -- ^ TLS connection
  | UseInsecureTls -- ^ TLS connection without certificate checking
  | UseInsecure    -- ^ Plain connection
  deriving Show

-- | Fingerprint used to validate server certificates.
data Fingerprint
  = FingerprintSha1   ByteString -- ^ SHA-1 fingerprint
  | FingerprintSha256 ByteString -- ^ SHA-2 256-bit fingerprint
  | FingerprintSha512 ByteString -- ^ SHA-2 512-bit fingerprint
  deriving Show

makeLenses ''ServerSettings
makePrisms ''SaslMechanism

-- | The defaults for server settings.
defaultServerSettings :: ServerSettings
defaultServerSettings =
  ServerSettings
       { _ssNicks         = pure "guest"
       , _ssUser          = "username"
       , _ssReal          = "realname"
       , _ssPassword      = Nothing
       , _ssSaslMechanism = Nothing
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
       , _ssReconnectError   = Nothing
       , _ssAutoconnect      = False
       , _ssNickCompletion   = defaultNickWordCompleteMode
       , _ssLogDir           = Nothing
       , _ssProtocolFamily   = Nothing
       , _ssSts              = True
       , _ssTlsPubkeyFingerprint = Nothing
       , _ssTlsCertFingerprint   = Nothing
       , _ssShowAccounts     = False
       , _ssCapabilities     = []
       }

serverSpec :: ValueSpec (ServerSettings -> ServerSettings)
serverSpec = sectionsSpec "server-settings" $
  composeMaybe <$> sequenceA settings
  where

    composeMaybe :: [Maybe (a -> a)] -> a -> a
    composeMaybe = ala Endo (foldMap . foldMap)

    req name l s info
      = optSection' name ?? info
      $ set l <$> s

    opt name l s info
      = optSection' name ?? info
      $ set l . Just <$> s <!>
        set l Nothing <$ atomSpec "clear"

    settings :: [SectionsSpec (Maybe (ServerSettings -> ServerSettings))]
    settings =
      [ opt "name" ssName anySpec
        "The name used to identify this server in the client"

      , req "hostname" ssHostName stringSpec
        "Hostname of server"

      , opt "port" ssPort numSpec
        "Port number of server. Default 6667 without TLS or 6697 with TLS"

      , req "nick" ssNicks nicksSpec
        "Nicknames to connect with in order"

      , opt "password" ssPassword anySpec
        "Server password"

      , req "username" ssUser anySpec
        "Second component of _!_@_ usermask"

      , req "realname" ssReal anySpec
        "\"GECOS\" name sent to server visible in /whois"

      , opt "sasl" ssSaslMechanism saslMechanismSpec
        "SASL settings"

      , req "tls" ssTls useTlsSpec
        "Set to `yes` to enable secure connect. Set to `yes-insecure` to disable certificate checking."

      , opt "tls-client-cert" ssTlsClientCert stringSpec
        "Path to TLS client certificate"

      , opt "tls-client-key" ssTlsClientKey stringSpec
        "Path to TLS client key"

      , opt "tls-server-cert" ssTlsServerCert stringSpec
        "Path to CA certificate bundle"

      , req "tls-ciphers" ssTlsCiphers stringSpec
        "OpenSSL cipher specification. Default to \"HIGH\""

      , opt "socks-host" ssSocksHost stringSpec
        "Hostname of SOCKS5 proxy server"

      , req "socks-port" ssSocksPort numSpec
        "Port number of SOCKS5 proxy server"

      , req "connect-cmds" ssConnectCmds (listSpec macroCommandSpec)
        "Command to be run upon successful connection to server"

      , req "chanserv-channels" ssChanservChannels (listSpec identifierSpec)
        "Channels with ChanServ permissions available"

      , req "flood-penalty" ssFloodPenalty anySpec
        "RFC 1459 rate limiting, seconds of penalty per message (default 2)"

      , req "flood-threshold" ssFloodThreshold anySpec
        "RFC 1459 rate limiting, seconds of allowed penalty accumulation (default 10)"

      , req "message-hooks" ssMessageHooks (listSpec hookSpec)
        "Special message hooks to enable: \"buffextras\" available"

      , req "reconnect-attempts" ssReconnectAttempts anySpec
        "Number of reconnection attempts on lost connection"

      , opt "reconnect-error" ssReconnectError regexSpec
        "Regular expression for disconnect messages that trigger reconnect."

      , req "autoconnect" ssAutoconnect yesOrNoSpec
        "Set to `yes` to automatically connect at client startup"

      , req "nick-completion" ssNickCompletion nickCompletionSpec
        "Behavior for nickname completion with TAB"

      , opt "log-dir" ssLogDir stringSpec
        "Path to log file directory for this server"

      , opt "protocol-family" ssProtocolFamily protocolFamilySpec
        "IP protocol family to use for this connection"

      , req "sts" ssSts yesOrNoSpec
        "Honor server STS policies forcing TLS connections"

      , opt "tls-cert-fingerprint" ssTlsCertFingerprint fingerprintSpec
        "Check SHA1, SHA256, or SHA512 certificate fingerprint"

      , opt "tls-pubkey-fingerprint" ssTlsPubkeyFingerprint fingerprintSpec
        "Check SHA1, SHA256, or SHA512 public key fingerprint"

      , req "show-accounts" ssShowAccounts yesOrNoSpec
        "Render account names alongside chat messages"

      , req "capabilities" ssCapabilities anySpec
        "Extra capabilities to unconditionally request from the server"
      ]

saslMechanismSpec :: ValueSpec SaslMechanism
saslMechanismSpec = plain <!> external <!> ecdsa
  where
    mech m   = reqSection' "mechanism" (atomSpec m) "Mechanism"
    authzid  = optSection "authzid" "Authorization identity"
    username = reqSection "username" "Authentication identity"

    plain =
      sectionsSpec "sasl-plain" $ SaslPlain <$
      optSection' "mechanism" (atomSpec "plain") "Mechanism" <*>
      authzid <*> username <*> reqSection "password" "Password"

    external =
      sectionsSpec "sasl-external" $ SaslExternal <$ mech "external" <*>
      authzid

    ecdsa =
      sectionsSpec "sasl-ecdsa-nist256p-challenge-mech" $
      SaslEcdsa <$ mech "ecdsa-nist256p-challenge" <*>
      authzid <*> username <*>
      reqSection' "private-key" stringSpec "Private key file"

hookSpec :: ValueSpec HookConfig
hookSpec =
  flip HookConfig [] <$> anySpec <!>
  (\(x:|xs) -> HookConfig x xs) <$> nonemptySpec anySpec

-- | Match fingerprints in plain hex or colon-delimited bytes.
-- SHA-1 is 20 bytes. SHA-2-256 is 32 bytes. SHA-2-512 is 64 bytes.
--
-- @
-- 00112233aaFF
-- 00:11:22:33:aa:FF
-- @
fingerprintSpec :: ValueSpec Fingerprint
fingerprintSpec =
  customSpec "fingerprint" stringSpec $ \str ->
    do bytes <- B.pack <$> traverse readWord8 (byteStrs str)
       case B.length bytes of
         20 -> Right (FingerprintSha1   bytes)
         32 -> Right (FingerprintSha256 bytes)
         64 -> Right (FingerprintSha512 bytes)
         _  -> Left "expected 20, 32, or 64 bytes"
  where
    -- read a single byte in hex
    readWord8 i =
      case readHex i of
        [(x,"")]
          | 0 <= x, x < 256 -> Right (fromIntegral (x :: Integer))
          | otherwise -> Left "byte out-of-bounds"
        _ -> Left "bad hex-encoded byte"

    byteStrs :: String -> [String]
    byteStrs str
      | ':' `elem` str = splitOn ":" str
      | otherwise      = chunksOf 2  str

-- | Specification for IP protocol family.
protocolFamilySpec :: ValueSpec Family
protocolFamilySpec =
      AF_INET   <$ atomSpec "inet"
  <!> AF_INET6  <$ atomSpec "inet6"


nicksSpec :: ValueSpec (NonEmpty Text)
nicksSpec = oneOrNonemptySpec anySpec


useTlsSpec :: ValueSpec UseTls
useTlsSpec =
      UseTls         <$ atomSpec "yes"
  <!> UseInsecureTls <$ atomSpec "yes-insecure"
  <!> UseInsecure    <$ atomSpec "no"


nickCompletionSpec :: ValueSpec WordCompletionMode
nickCompletionSpec =
      defaultNickWordCompleteMode <$ atomSpec "default"
  <!> slackNickWordCompleteMode   <$ atomSpec "slack"


identifierSpec :: ValueSpec Identifier
identifierSpec = mkId <$> anySpec

regexSpec :: ValueSpec KnownRegex
regexSpec = customSpec "regex" anySpec $ \str ->
  case compile defaultCompOpt ExecOption{captureGroups = False} str of
    Left e  -> Left  (Text.pack e)
    Right r -> Right (KnownRegex str r)
