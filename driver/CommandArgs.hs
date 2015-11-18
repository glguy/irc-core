{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module CommandArgs where

import Config
import Config.Lens
import Control.Applicative
import Control.Exception
import Control.Monad (when, unless)
import Data.Foldable (traverse_)
import Data.Maybe
import Data.List (foldl')
import Data.Text (Text)
import Data.Text.Lens (unpacked)
import Data.Version (showVersion)
import Network.Socket (PortNumber)
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import Control.Lens
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import ServerSettings
import Paths_irc_core

defaultSocksPort :: PortNumber
defaultSocksPort = 1080

defaultConfigPath :: IO FilePath
defaultConfigPath =
  do dir <- getAppUserDataDirectory "glirc"
     return (dir </> "config")

data CommandArgs = CommandArgs
  { _cmdArgNick     :: Maybe String
  , _cmdArgServer   :: String
  , _cmdArgPort     :: Maybe Int
  , _cmdArgHelp     :: Bool
  , _cmdArgVersion  :: Bool
  , _cmdArgReal     :: Maybe String
  , _cmdArgUser     :: Maybe String
  , _cmdArgSaslUser :: Maybe String
  , _cmdArgDebug    :: Maybe FilePath
  , _cmdArgUserInfo :: Maybe String
  , _cmdArgTls      :: Bool
  , _cmdArgTlsClientCert :: Maybe FilePath
  , _cmdArgTlsClientKey  :: Maybe FilePath
  , _cmdArgTlsInsecure   :: Bool
  , _cmdArgConfigFile :: Maybe FilePath
  , _cmdArgConfigValue:: Value
  }

makeLenses ''CommandArgs

emptyCommandArgs :: CommandArgs
emptyCommandArgs = CommandArgs
  { _cmdArgServer   = ""
  , _cmdArgReal     = Nothing
  , _cmdArgUser     = Nothing
  , _cmdArgNick     = Nothing
  , _cmdArgUserInfo = Nothing

  , _cmdArgSaslUser = Nothing
  , _cmdArgPort     = Nothing
  , _cmdArgHelp     = False
  , _cmdArgVersion  = False
  , _cmdArgDebug    = Nothing
  , _cmdArgTls      = False
  , _cmdArgTlsClientCert = Nothing
  , _cmdArgTlsClientKey  = Nothing
  , _cmdArgTlsInsecure   = False
  , _cmdArgConfigFile    = Nothing
  , _cmdArgConfigValue   = Sections []
  }

getCommandArgs :: IO CommandArgs
getCommandArgs =
  do args <- getArgs

     let (flags, servers, errors) = getOpt Permute optDescrs args
         r = foldl' (\acc f -> f acc) emptyCommandArgs flags

     when (view cmdArgHelp r) help
     when (view cmdArgVersion r) emitVersion
     unless (null errors) $
       do traverse_ (hPutStrLn stderr) errors
          exitFailure
     server <- case servers of
       [server] -> return server
       [] -> do hPutStrLn stderr "Expected server name argument (try --help)"
                exitFailure
       _  -> do hPutStrLn stderr "Too many server name arguments (try --help)"
                exitFailure

     v <- loadConfigValue (view cmdArgConfigFile r)

     return $ set cmdArgServer server
            $ set cmdArgConfigValue v
            $ r

help :: IO a
help =
  do prog <- getProgName
     let txt = prog ++ " <options> SERVER"
     hPutStr stderr (usageInfo txt optDescrs)
     exitFailure

emitVersion :: IO a
emitVersion =
  do putStrLn ("glirc " ++ showVersion version)
     exitSuccess

optDescrs :: [OptDescr (CommandArgs -> CommandArgs)]
optDescrs =
  [ Option "c" [ "config"]   (ReqArg (set cmdArgConfigFile . Just) "FILENAME") "Configuration file path (default ~/.glirc/config)"
  , Option "p" [ "port"]     (ReqArg (set cmdArgPort     . Just . read) "PORT") "IRC Server Port"
  , Option "n" [ "nick"]     (ReqArg (set cmdArgNick     . Just) "NICK") "Nickname"
  , Option "u" [ "user"]     (ReqArg (set cmdArgUser     . Just) "USER") "Username"
  , Option "r" [ "real"]     (ReqArg (set cmdArgReal     . Just) "REAL") "Real Name"
  , Option ""  ["sasl-user"] (ReqArg (set cmdArgSaslUser . Just) "USER") "SASL username"
  , Option "d" [ "debug"]    (ReqArg (set cmdArgDebug    . Just) "FILE") "Debug log filename"
  , Option "i" [ "userinfo"] (ReqArg (set cmdArgUserInfo . Just) "USERINFO") "CTCP USERINFO Response"
  , Option "t" [ "tls"]      (NoArg  (set cmdArgTls True)) "Enable TLS"
  , Option ""  [ "tls-client-cert"] (ReqArg (set cmdArgTlsClientCert . Just) "PATH") "Path to PEM encoded client certificate"
  , Option ""  [ "tls-client-key"] (ReqArg (set cmdArgTlsClientKey . Just) "PATH") "Path to PEM encoded client key"
  , Option ""  [ "tls-insecure"] (NoArg (set cmdArgTlsInsecure True)) "Disable server certificate verification"
  , Option "v" [ "version"]   (NoArg  (set cmdArgVersion True))   "Show version"
  , Option "h" [ "help"]      (NoArg  (set cmdArgHelp True))   "Show help"
  ]

initialServerSettings :: CommandArgs -> IO ServerSettings
initialServerSettings !args =
  do env  <- getEnvironment
     let username     = fromMaybe "" (lookup "USER" env)
         password     = lookup "IRCPASSWORD" env
         saslpassword = lookup "SASLPASSWORD" env
         nick         = fromMaybe username
                           (view cmdArgNick args <|> defaultStr hostTxt "nick" args)
         hostTxt      = Text.pack (view cmdArgServer args)

     return ServerSettings
       { _ssNick           = nick
       , _ssUser           = fromMaybe username
                           $ view cmdArgUser args
                         <|> defaultStr hostTxt "username" args

       , _ssReal           = fromMaybe username
                           $ view cmdArgReal args
                         <|> defaultStr hostTxt "realname" args

       , _ssUserInfo       = fromMaybe username
                           $ view cmdArgUserInfo args
                         <|> defaultStr hostTxt "userinfo" args

       , _ssPassword       = password
                         <|> defaultStr hostTxt "password" args

       , _ssSaslCredential = (saslpassword <|> defaultStr hostTxt "sasl-password" args)
                         <&> \p -> (fromMaybe nick (view cmdArgSaslUser args
                                                <|> defaultStr hostTxt "sasl-username" args), p)

       , _ssHostName       = view cmdArgServer args

       , _ssPort           = fromIntegral <$> view cmdArgPort args
                         <|> fromIntegral <$> defaultNum hostTxt "port" args

       , _ssTls            = view cmdArgTls args
                          || fromMaybe False (defaultBool hostTxt "tls" args)

       , _ssTlsInsecure    = view cmdArgTlsInsecure args

       , _ssTlsClientCert  = view cmdArgTlsClientCert args
                         <|> defaultStr hostTxt "tls-client-cert" args

       , _ssTlsClientKey   = view cmdArgTlsClientKey args
                         <|> defaultStr hostTxt "tls-client-key" args

       , _ssConnectCmds   = toListOf (cmdArgConfigValue . configPath hostTxt "connect-cmds"
                                     . values . text) args

       , _ssSocksProxy    = do h <- defaultStr hostTxt "socks-host" args
                               let p = maybe defaultSocksPort fromIntegral
                                     $ defaultNum hostTxt "socks-port" args
                               return (h,p)

       , _ssServerCerts   = toListOf
                              ( cmdArgConfigValue
                              . configPath hostTxt "server-certificates"
                              . failing values id -- allow both list and singleton
                              . text
                              . unpacked
                              ) args
       }


loadConfigValue :: Maybe FilePath -> IO Value
loadConfigValue mbFp =
  case mbFp of
    Just fp -> process fp
    Nothing ->
      do fp <- defaultConfigPath
         process fp
           `catch` \e ->
           if isDoesNotExistError e
             then return emptyConfig
             else throwIO e
  where
  emptyConfig = Sections []

  process fp =
    do raw <- Text.readFile fp
       case parse raw of
         Right v -> return v
         Left errMsg ->
           do hPutStrLn stderr "Configuration error"
              hPutStrLn stderr (fp ++ ":" ++ errMsg)
              exitFailure

-- | Apply a function to the 'Bool' contained inside the given
-- 'Value' when it is a @Bool@.
bool :: Applicative f => (Bool -> f Bool) -> Value -> f Value
bool = atom . _Bool

-- | Map 'True' to @"yes"@ and 'False' to @"no"@
_Bool :: Prism' Atom Bool
_Bool = prism'
          (\b -> if b then "yes" else "no")
          (\a -> case a of
                   "yes" -> Just True
                   "no"  -> Just False
                   _     -> Nothing)

------------------------------------------------------------------------
-- Look up settings for a given server from the config
------------------------------------------------------------------------

hostnameMatch :: Text -> Value -> Bool
hostnameMatch = elemOf (key "hostname" . text)

configPath :: (Applicative f, Contravariant f) => Text -> Text -> LensLike' f Value Value
configPath hostname name =
  failing (key "servers" . values . filtered (hostnameMatch hostname) . key name)
          (key "defaults" . key name)

defaultStr :: Text -> Text -> CommandArgs -> Maybe String
defaultStr  hostname i = preview (cmdArgConfigValue . configPath hostname i . text . unpacked)

defaultBool :: Text -> Text -> CommandArgs -> Maybe Bool
defaultBool hostname i = preview (cmdArgConfigValue . configPath hostname i . bool)

defaultNum :: Text -> Text -> CommandArgs -> Maybe Integer
defaultNum  hostname i = preview (cmdArgConfigValue . configPath hostname i . number)
