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
import Data.Text (Text)
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
import qualified Data.Text as Text
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
       Left errMsg ->
         do hPutStrLn stderr "Configuration file parse error"
            hPutStrLn stderr (fp ++ ":" ++ errMsg)
            exitFailure

-- | Apply a function to the 'Bool' contained inside the given
-- 'Value' when it is a @Bool@.
bool :: Applicative f => (Bool -> f Bool) -> Value -> f Value
bool = atom . boolAtom

-- | Traverse 'Text' as a boolean when it is @"yes"@ or @"no"@.
boolAtom :: Applicative f => (Bool -> f Bool) -> Text -> f Text
boolAtom f "yes" = boolText <$> f True
boolAtom f "no"  = boolText <$> f False
boolAtom _ t     = pure t

-- | Map 'True' to @"yes"@ and 'False' to @"no"@
boolText :: Bool -> Text
boolText True  = "yes"
boolText False = "no"

------------------------------------------------------------------------
-- Look up settings for a given server from the config
------------------------------------------------------------------------

hostnameMatch :: Text -> Value -> Bool
hostnameMatch = elemOf (key "hostname" . text)

configPath :: (Applicative f, Contravariant f) => Text -> Text -> LensLike' f Value Value
configPath hostname name =
  failing (key "servers" . list . folded . filtered (hostnameMatch hostname) . key name)
          (key "defaults" . key name)

defaultStr :: Text -> Text -> CommandArgs -> Maybe String
defaultStr  hostname i = preview (cmdArgConfigValue . configPath hostname i . text . unpacked)

defaultBool :: Text -> Text -> CommandArgs -> Maybe Bool
defaultBool hostname i = preview (cmdArgConfigValue . configPath hostname i . bool)

defaultNum :: Text -> Text -> CommandArgs -> Maybe Integer
defaultNum  hostname i = preview (cmdArgConfigValue . configPath hostname i . number)
