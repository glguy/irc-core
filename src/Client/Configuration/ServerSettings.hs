{-# LANGUAGE LambdaCase, ApplicativeDo, TemplateHaskell, OverloadedStrings, RecordWildCards, BlockArguments #-}

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
  , ssTlsVerify
  , ssTlsClientCert
  , ssTlsClientKey
  , ssTlsClientKeyPassword
  , ssTlsServerCert
  , ssTlsCiphers
  , ssTls13Ciphers
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
  , ssBindHostName
  , ssSts
  , ssTlsPubkeyFingerprint
  , ssTlsCertFingerprint
  , ssShowAccounts
  , ssCapabilities
  , ssWindowHints

  -- * SASL Mechanisms
  , SaslMechanism(..)
  , _SaslExternal
  , _SaslEcdsa
  , _SaslPlain
  , _SaslScram

  -- * Secrets
  , Secret(..)
  , SecretException(..)
  , loadSecrets

  -- * Defaults
  , defaultServerSettings

  -- * TLS settings
  , UseTls(..)
  , Fingerprint(..)
  , TlsMode(..)

  -- * Regex wrapper
  , KnownRegex(..)
  , getRegex
  ) where

import           Client.Authentication.Scram (ScramDigest(..))
import           Client.Commands.Interpolation
import           Client.Commands.WordCompletion
import           Client.Configuration.Macros (macroCommandSpec)
import           Client.State.Focus ( Focus (NetworkFocus, ChannelFocus) )
import           Config.Schema.Spec
import           Control.Exception (Exception, displayException, throwIO, try)
import           Control.Lens
import           Control.Monad ((>=>))
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.Split (chunksOf, splitOn)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import           Irc.Identifier (Identifier, mkId)
import           Network.Socket (HostName, PortNumber)
import           Numeric (readHex)
import qualified System.Exit as Exit
import qualified System.Process as Process
import           Text.Regex.TDFA
import           Text.Regex.TDFA.Text (compile)
import           Hookup (TlsVerify(..))

-- | Static server-level settings
data ServerSettings = ServerSettings
  { _ssNicks            :: !(NonEmpty Text) -- ^ connection nicknames
  , _ssUser             :: !Text -- ^ connection username
  , _ssReal             :: !Text -- ^ connection realname / GECOS
  , _ssPassword         :: !(Maybe Secret) -- ^ server password
  , _ssSaslMechanism    :: !(Maybe SaslMechanism) -- ^ SASL mechanism
  , _ssHostName         :: !HostName -- ^ server hostname
  , _ssPort             :: !(Maybe PortNumber) -- ^ server port
  , _ssTls              :: !TlsMode -- ^ use TLS to connect
  , _ssTlsVerify        :: !TlsVerify -- ^ verify TLS hostname
  , _ssTlsClientCert    :: !(Maybe FilePath) -- ^ path to client TLS certificate
  , _ssTlsClientKey     :: !(Maybe FilePath) -- ^ path to client TLS key
  , _ssTlsClientKeyPassword :: !(Maybe Secret) -- ^ client key PEM password
  , _ssTlsServerCert    :: !(Maybe FilePath) -- ^ additional CA certificates for validating server
  , _ssTlsCiphers       :: String            -- ^ OpenSSL cipher suite
  , _ssTls13Ciphers     :: Maybe String      -- ^ OpenSSL TLS 1.3 cipher suite
  , _ssTlsPubkeyFingerprint :: !(Maybe Fingerprint) -- ^ optional acceptable public key fingerprint
  , _ssTlsCertFingerprint   :: !(Maybe Fingerprint) -- ^ optional acceptable certificate fingerprint
  , _ssSts              :: !Bool -- ^ Honor STS policies when true
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
  , _ssBindHostName     :: Maybe HostName -- ^ Local bind host
  , _ssShowAccounts     :: !Bool -- ^ Render account names
  , _ssCapabilities     :: ![Text] -- ^ Extra capabilities to unconditionally request
  , _ssWindowHints      :: Map Focus Char
  }
  deriving Show

data TlsMode = TlsYes | TlsNo | TlsStart
  deriving Show

data Secret
  = SecretText Text    -- ^ Constant text
  | SecretCommand (NonEmpty Text) -- ^ Command to generate text
  deriving Show

-- | SASL mechanisms and configuration data.
data SaslMechanism
  = SaslPlain    (Maybe Text) Text Secret -- ^ SASL PLAIN RFC4616 - authzid authcid password
  | SaslEcdsa    (Maybe Text) Text FilePath -- ^ SASL NIST - https://github.com/kaniini/ecdsatool - authzid keypath
  | SaslExternal (Maybe Text)      -- ^ SASL EXTERNAL RFC4422 - authzid
  | SaslScram    ScramDigest (Maybe Text) Text Secret -- ^ SASL SCRAM-SHA-256 RFC7677 - authzid authcid password
  | SaslEcdh     (Maybe Text) Text Secret -- ^ SASL ECDH-X25519-CHALLENGE - authzid authcid private-key
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
       , _ssTls           = TlsNo
       , _ssTlsVerify     = VerifyDefault
       , _ssTlsClientCert = Nothing
       , _ssTlsClientKey  = Nothing
       , _ssTlsClientKeyPassword = Nothing
       , _ssTlsServerCert = Nothing
       , _ssTlsCiphers    = "HIGH"
       , _ssTls13Ciphers  = Nothing
       , _ssTlsPubkeyFingerprint = Nothing
       , _ssTlsCertFingerprint   = Nothing
       , _ssSts              = True
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
       , _ssBindHostName     = Nothing
       , _ssShowAccounts     = False
       , _ssCapabilities     = []
       , _ssWindowHints      = Map.empty
       }

serverSpec :: ValueSpec (Maybe Text, ServerSettings -> ServerSettings)
serverSpec = sectionsSpec "server-settings" $
  do mbExt <- optSection "extends" "name of a server to use for defaults"
     upd <- composeMaybe <$> sequenceA settings
     pure (mbExt, upd)
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

      , req "tls" ssTls tlsModeSpec
        "Use TLS to connect (default no)"

      , req "tls-verify" ssTlsVerify tlsVerifySpec
        "Enable server certificate hostname verification (default yes, string to override hostname)"

      , opt "tls-client-cert" ssTlsClientCert filepathSpec
        "Path to TLS client certificate"

      , opt "tls-client-key" ssTlsClientKey filepathSpec
        "Path to TLS client key"

      , opt "tls-client-key-password" ssTlsClientKeyPassword anySpec
        "Password for decrypting TLS client key PEM file"

      , opt "tls-server-cert" ssTlsServerCert filepathSpec
        "Path to CA certificate bundle"

      , req "tls-ciphers" ssTlsCiphers stringSpec
        "OpenSSL cipher specification. Default to \"HIGH\""

      , opt "tls-1.3-ciphers" ssTls13Ciphers stringSpec
        "OpenSSL TLS 1.3 cipher specification."

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

      , opt "log-dir" ssLogDir filepathSpec
        "Path to log file directory for this server"

      , opt "bind-hostname" ssBindHostName stringSpec
        "Source address to bind to before connecting"

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

      , req "window-hints" ssWindowHints windowHintsSpec
        "Hints for naming windows"
      ]

windowHintsSpec :: ValueSpec (Map Focus Char)
windowHintsSpec = Map.fromList <$> listSpec entrySpec
  where
    entrySpec =
      sectionsSpec "window-hint"
        do focus  <- reqSection' "window" focusSpec ""
           hotkey <- reqSection' "hotkey" hotkeySpec ""
           pure (focus, hotkey)
    focusSpec =
      NetworkFocus "" <$ atomSpec "network" <!>
      ChannelFocus "" . mkId <$> textSpec
    hotkeySpec =
      customSpec "letter" stringSpec \case
        [x] -> Right x
        _   -> Left "expected a single letter"

tlsModeSpec :: ValueSpec TlsMode
tlsModeSpec =
  TlsYes   <$ atomSpec "yes"      <!>
  TlsNo    <$ atomSpec "no"       <!>
  TlsStart <$ atomSpec "starttls"

tlsVerifySpec :: ValueSpec TlsVerify
tlsVerifySpec =
  VerifyDefault  <$ atomSpec "yes"      <!>
  VerifyNone     <$ atomSpec "no"       <!>
  VerifyHostname <$> stringSpec

saslMechanismSpec :: ValueSpec SaslMechanism
saslMechanismSpec = plain <!> external <!> ecdsa <!> scram <!> ecdh
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
      reqSection' "private-key" filepathSpec "Private key file"

    scramDigest =
      fromMaybe ScramDigestSha2_256 <$>
      optSection' "digest" scramDigests "Underlying digest function"

    scramDigests =
      ScramDigestSha1     <$ atomSpec "sha1" <!>
      ScramDigestSha2_256 <$ atomSpec "sha2-256" <!>
      ScramDigestSha2_512 <$ atomSpec "sha2-512"

    scram =
      sectionsSpec "sasl-scram" $
      SaslScram <$ mech "scram" <*>
      scramDigest <*>
      authzid <*> username <*> reqSection "password" "Password"

    ecdh =
      sectionsSpec "sasl-ecdh-x25519-challenge" $
      SaslEcdh <$ mech "ecdh-x25519-challenge" <*>
      authzid <*> username <*> reqSection "private-key" "Private Key"




filepathSpec :: ValueSpec FilePath
filepathSpec = customSpec "path" stringSpec $ \str ->
  if null str
  then Left "empty path"
  else Right str

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

nicksSpec :: ValueSpec (NonEmpty Text)
nicksSpec = oneOrNonemptySpec anySpec

nickCompletionSpec :: ValueSpec WordCompletionMode
nickCompletionSpec =
      defaultNickWordCompleteMode <$ atomSpec "default"
  <!> slackNickWordCompleteMode   <$ atomSpec "slack"
  <!> customNickCompletion

customNickCompletion :: ValueSpec WordCompletionMode
customNickCompletion =
  sectionsSpec "nick-completion" $
  do wcmStartPrefix  <- fromMaybe "" <$> optSection' "start-prefix" stringSpec
                        "Prefix for nickname with when completing at start of line."
     wcmStartSuffix  <- fromMaybe "" <$> optSection' "start-suffix" stringSpec
                        "Suffix for nickname with when completing at start of line."
     wcmMiddlePrefix <- fromMaybe "" <$> optSection' "middle-prefix" stringSpec
                        "Prefix for nickname with when completing in middle of line."
     wcmMiddleSuffix <- fromMaybe "" <$> optSection' "middle-suffix" stringSpec
                        "Suffix for nickname with when completing in middle of line."
     pure WordCompletionMode{..}


identifierSpec :: ValueSpec Identifier
identifierSpec = mkId <$> anySpec

regexSpec :: ValueSpec KnownRegex
regexSpec = customSpec "regex" anySpec $ \str ->
  case compile defaultCompOpt ExecOption{captureGroups = False} str of
    Left e  -> Left  (Text.pack e)
    Right r -> Right (KnownRegex str r)

instance HasSpec Secret where
  anySpec = SecretText <$> textSpec <!>
            SecretCommand <$> sectionsSpec "command" (reqSection "command" "Command and arguments to execute to secret")

data SecretException = SecretException String String
  deriving Show

instance Exception SecretException

-- | Run the secret commands in a server configuration replacing them with secret text.
-- Throws 'SecretException'
loadSecrets :: ServerSettings -> IO ServerSettings
loadSecrets =
  traverseOf (ssPassword             . _Just                  ) (loadSecret "server password") >=>
  traverseOf (ssSaslMechanism        . _Just . _SaslPlain . _3) (loadSecret "SASL password") >=>
  traverseOf (ssTlsClientKeyPassword . _Just                  ) (loadSecret "TLS key password") >=>
  traverseOf (ssSaslMechanism        . _Just . _SaslScram . _4) (loadSecret "SASL password") >=>
  traverseOf (ssSaslMechanism        . _Just . _SaslEcdh  . _3) (loadSecret "SASL private key")

-- | Run a command if found and replace it with the first line of stdout result.
loadSecret :: String -> Secret -> IO Secret
loadSecret _ (SecretText txt) = pure (SecretText txt)
loadSecret label (SecretCommand (cmd NonEmpty.:| args)) =
  do let u = Text.unpack
     res <- try (Process.readProcessWithExitCode (u cmd) (map u args) "")
     case res of
       Right (Exit.ExitSuccess,out,_) -> pure (SecretText (Text.pack (takeWhile ('\n' /=) out)))
       Right (Exit.ExitFailure{},_,err) -> throwIO (SecretException label err)
       Left ioe -> throwIO (SecretException label (displayException (ioe::IOError)))
