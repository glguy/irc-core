{-# LANGUAGE TemplateHaskell #-}
module CommandArgs where

import Data.Foldable (traverse_)
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Control.Lens

import ServerSettings

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
                           }
                  return (foldl (\acc f -> f acc) r0 fs)

            (_ , _, errs) ->
                 do traverse_ (hPutStrLn stderr) errs
                    help

     if view cmdArgHelp r
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
  [ Option "p" [ "port"]     (ReqArg (set cmdArgPort     . Just . read) "PORT") "IRC Server Port"
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
initialServerSettings args =
  do env  <- getEnvironment
     let username     = fromMaybe "" (lookup "USER" env)
         password     = lookup "IRCPASSWORD" env
         saslpassword = lookup "SASLPASSWORD" env
         nick         = fromMaybe username (view cmdArgNick args)
     return ServerSettings
       { _ssNick           = nick
       , _ssUser           = fromMaybe username (view cmdArgUser args)
       , _ssReal           = fromMaybe username (view cmdArgReal args)
       , _ssUserInfo       = fromMaybe username (view cmdArgUserInfo args)
       , _ssPassword       = password
       , _ssSaslCredential = saslpassword <&> \p ->
                               (fromMaybe nick (view cmdArgSaslUser args), p)
       , _ssHostName       = view cmdArgServer args
       , _ssPort           = fmap fromIntegral (view cmdArgPort args)
       , _ssTls            = view cmdArgTls args
       , _ssTlsInsecure    = view cmdArgTlsInsecure args
       , _ssTlsClientCert  = view cmdArgTlsClientCert args
       , _ssTlsClientKey   = view cmdArgTlsClientKey args
       }
