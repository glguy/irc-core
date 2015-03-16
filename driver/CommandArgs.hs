{-# LANGUAGE TemplateHaskell #-}
module CommandArgs where

import Data.Foldable (traverse_)
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Control.Lens

data CommandArgs = CommandArgs
  { _cmdArgNick     :: String
  , _cmdArgServer   :: String
  , _cmdArgPort     :: Int
  , _cmdArgPassword :: Maybe String
  , _cmdArgHelp     :: Bool
  , _cmdArgReal     :: String
  , _cmdArgUser     :: String
  , _cmdArgSaslUser :: String
  , _cmdArgSaslPass :: Maybe String
  , _cmdArgDebug    :: Maybe FilePath
  , _cmdArgUserInfo :: String
  , _cmdArgTls      :: Bool
  , _cmdArgTlsClientCert :: Maybe FilePath
  , _cmdArgTlsClientKey  :: Maybe FilePath
  , _cmdArgTlsInsecure   :: Bool
  }

data SslArgs = SslArgs
  { _sslClientCertificate :: Maybe FilePath
  }

makeLenses ''CommandArgs

initialCommandArgs :: CommandArgs
initialCommandArgs = CommandArgs
  { _cmdArgNick     = ""
  , _cmdArgServer   = ""
  , _cmdArgUser     = ""
  , _cmdArgReal     = ""
  , _cmdArgPort     = 6667
  , _cmdArgPassword = Nothing
  , _cmdArgHelp     = False
  , _cmdArgSaslUser = ""
  , _cmdArgSaslPass = Nothing
  , _cmdArgDebug    = Nothing
  , _cmdArgUserInfo = ""
  , _cmdArgTls      = False
  , _cmdArgTlsClientCert = Nothing
  , _cmdArgTlsClientKey  = Nothing
  , _cmdArgTlsInsecure   = False
  }

getCommandArgs :: IO CommandArgs
getCommandArgs =
  do args <- getArgs
     env  <- getEnvironment
     let username = fromMaybe "" (lookup "USER" env)
         password = lookup "IRCPASSWORD" env
         saslpassword = lookup "SASLPASSWORD" env

     r <- case getOpt RequireOrder optDescrs args of
            (fs, [server], []) ->
               do let r0 = initialCommandArgs
                           { _cmdArgServer   = server
                           , _cmdArgPassword = password
                           , _cmdArgReal     = username
                           , _cmdArgUser     = username
                           , _cmdArgNick     = username
                           , _cmdArgSaslUser = username
                           , _cmdArgSaslPass = saslpassword
                           }
                  return (foldl (\acc f -> f acc) r0 fs)

            (_ , _, errs) ->
                 do traverse_ (hPutStrLn stderr) errs
                    help

     if _cmdArgHelp r
        then help
        else return r

help :: IO a
help =
  do prog <- getProgName
     let txt = prog ++ " <options> SERVER"
     hPutStr stderr (usageInfo txt optDescrs)
     exitFailure

optDescrs :: [OptDescr (CommandArgs -> CommandArgs)]
optDescrs =
  [ Option "p" [ "port"]      (ReqArg (set cmdArgPort . read) "PORT") "IRC Server Port"
  , Option "n" [ "nick"]      (ReqArg (set cmdArgNick) "NICK") "Nickname"
  , Option "u" [ "user"]      (ReqArg (set cmdArgUser) "USER") "Username"
  , Option "r" [ "real"]      (ReqArg (set cmdArgReal) "REAL") "Real Name"
  , Option ""  ["sasl-user"] (ReqArg (set cmdArgSaslUser) "USER") "SASL username"
  , Option "d" [ "debug"]     (ReqArg (set cmdArgDebug . Just) "FILE") "Debug log filename"
  , Option "i" [ "userinfo"]  (ReqArg (set cmdArgUserInfo) "USERINFO") "CTCP USERINFO Response"
  , Option "t" [ "tls"]       (NoArg  (set cmdArgTls True)) "Enable TLS"
  , Option ""  [ "tls-client-cert"] (ReqArg (set cmdArgTlsClientCert . Just) "PATH") "Path to PEM encoded client certificate"
  , Option ""  [ "tls-client-key"] (ReqArg (set cmdArgTlsClientKey . Just) "PATH") "Path to PEM encoded client key"
  , Option ""  [ "tls-insecure"] (NoArg (set cmdArgTlsInsecure True)) "Disable server certificate verification"
  , Option "h" [ "help"]      (NoArg  (set cmdArgHelp True))   "Show help"
  ]
