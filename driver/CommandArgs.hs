{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module CommandArgs where

import Config
import Config.Lens
import Control.Applicative
import Control.Exception
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Text.Lens (unpacked)
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import Control.Lens
import Config (Value(Sections),parse)
import qualified Data.Text.IO as Text

import ServerSettings

defaultConfigPath :: IO FilePath
defaultConfigPath =
  do dir <- getAppUserDataDirectory "glirc"
     return (dir </> "config")

data CommandArgs = CommandArgs
  { _cmdArgNick     :: Maybe String
  , _cmdArgServer   :: String
  , _cmdArgPort     :: Maybe Int
  , _cmdArgHelp     :: Bool
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

getCommandArgs :: IO CommandArgs
getCommandArgs =
  do args <- getArgs

     r <- case getOpt RequireOrder optDescrs args of
            (fs, [server], []) ->
               do let r0 = CommandArgs
                           { _cmdArgServer   = server
                           , _cmdArgReal     = Nothing
                           , _cmdArgUser     = Nothing
                           , _cmdArgNick     = Nothing
                           , _cmdArgUserInfo = Nothing

                           , _cmdArgSaslUser = Nothing
                           , _cmdArgPort     = Nothing
                           , _cmdArgHelp     = False
                           , _cmdArgDebug    = Nothing
                           , _cmdArgTls      = False
                           , _cmdArgTlsClientCert = Nothing
                           , _cmdArgTlsClientKey  = Nothing
                           , _cmdArgTlsInsecure   = False
                           , _cmdArgConfigFile    = Nothing
                           , _cmdArgConfigValue   = Sections []
                           }
                  return (foldl (\acc f -> f acc) r0 fs)

            (_ , _, errs) ->
                 do traverse_ (hPutStrLn stderr) errs
                    help

     when (view cmdArgHelp r) help

     v <- loadConfigValue (view cmdArgConfigFile r)
     return (set cmdArgConfigValue v r)

help :: IO a
help =
  do prog <- getProgName
     let txt = prog ++ " <options> SERVER"
     hPutStr stderr (usageInfo txt optDescrs)
     exitFailure

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
  , Option "h" [ "help"]      (NoArg  (set cmdArgHelp True))   "Show help"
  ]

initialServerSettings :: CommandArgs -> IO ServerSettings
initialServerSettings !args =
  do env  <- getEnvironment
     let username     = fromMaybe "" (lookup "USER" env)
         password     = lookup "IRCPASSWORD" env
         saslpassword = lookup "SASLPASSWORD" env
         nick         = fromMaybe username (view cmdArgNick args)

         defaultStr  i = preview (key "defaults" . key i . text . unpacked) (view cmdArgConfigValue args)
         defaultBool i = preview (key "defaults" . key i . bool) (view cmdArgConfigValue args)
         defaultNum  i = preview (key "defaults" . key i . number) (view cmdArgConfigValue args)

     return ServerSettings
       { _ssNick           = nick
       , _ssUser           = fromMaybe username
                               (view cmdArgUser args <|> defaultStr "username")
       , _ssReal           = fromMaybe username
                               (view cmdArgReal args <|> defaultStr "realname")
       , _ssUserInfo       = fromMaybe username
                               (view cmdArgUserInfo args <|> defaultStr "userinfo")
       , _ssPassword       = password <|> defaultStr "password"
       , _ssSaslCredential = (saslpassword <|> defaultStr "sasl-password")
                         <&> \p -> (fromMaybe nick (view cmdArgSaslUser args <|> defaultStr "sasl-username"), p)
       , _ssHostName       = view cmdArgServer args
       , _ssPort           = fromIntegral <$> view cmdArgPort args
                         <|> fromIntegral <$> defaultNum "port"
       , _ssTls            = view cmdArgTls args || fromMaybe False (defaultBool "tls")
       , _ssTlsInsecure    = view cmdArgTlsInsecure args
       , _ssTlsClientCert  = view cmdArgTlsClientCert args <|> defaultStr "tls-client-cert"
       , _ssTlsClientKey   = view cmdArgTlsClientKey args <|> defaultStr "tls-client-key"
       }

loadConfigValue :: Maybe FilePath -> IO Value
loadConfigValue mbFp =
  do fp  <- maybe defaultConfigPath return mbFp

     raw <- case mbFp of
              Just fp -> Text.readFile fp
              Nothing -> do fp <- defaultConfigPath
                            Text.readFile fp
                                `catch` \e -> if isDoesNotExistError e
                                              then return "{}"
                                              else throwIO e
     case parse raw of
       Right v -> return v
       Left (line,column) ->
         do hPutStrLn stderr $ "Configuration file parse error on line "
                            ++ show line ++ ", column "
                            ++ show column
            exitFailure
