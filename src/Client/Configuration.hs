{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language BangPatterns #-}
{-# Language RecordWildCards #-}
module Client.Configuration where

import           Client.ServerSettings
import           Control.Applicative
import           Config
import           Config.FromConfig
import           Control.Lens hiding (List)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Traversable
import           Network.Socket (HostName)
import           System.Directory
import           System.FilePath

data Configuration = Configuration
  { _configDefaults :: ServerSettings
  , _configServers  :: HashMap HostName ServerSettings
  }
  deriving Show

makeLenses ''Configuration

getConfigPath :: IO FilePath
getConfigPath =
  do dir <- getAppUserDataDirectory "glirc"
     return (dir </> "config")

loadConfiguration :: IO Configuration
loadConfiguration =
  do file <- Text.readFile =<< getConfigPath
     cfg  <- either fail return (parse file)
     def  <- loadDefaultServerSettings
     either (fail . Text.unpack) return (runConfigParser (parseConfiguration def cfg))


parseConfiguration :: ServerSettings -> Value -> ConfigParser Configuration
parseConfiguration def = parseSections $

  do _configDefaults <- fromMaybe def
                    <$> sectionOptWith (parseServerSettings def) "defaults"

     _configServers  <- fromMaybe HashMap.empty
                    <$> sectionOptWith (parseServers _configDefaults) "servers"

     return Configuration{..}

parseServers :: ServerSettings -> Value -> ConfigParser (HashMap HostName ServerSettings)
parseServers def (List xs) =
  do ys <- traverse (parseServerSettings def) xs
     return (HashMap.fromList [(view ssHostName ss, ss) | ss <- ys])
parseServers _ _ = failure "expected list"

sectionOptString :: Text -> SectionParser (Maybe String)
sectionOptString key = fmap Text.unpack <$> sectionOpt key

sectionOptStrings :: Text -> SectionParser (Maybe [String])
sectionOptStrings key = fmap (fmap Text.unpack) <$> sectionOpt key

sectionOptNum :: Num a => Text -> SectionParser (Maybe a)
sectionOptNum key = fmap fromInteger <$> sectionOpt key

parseServerSettings :: ServerSettings -> Value -> ConfigParser ServerSettings
parseServerSettings !def =
  parseSections $
    do _ssNick           <- fieldReq  ssNick          "nick"
       _ssUser           <- fieldReq  ssUser          "username"
       _ssReal           <- fieldReq  ssReal          "realname"
       _ssUserInfo       <- fieldReq  ssUserInfo      "userinfo"
       _ssPassword       <- field     ssPassword      "password"
       _ssSaslUsername   <- field     ssSaslUsername  "sasl-username"
       _ssSaslPassword   <- field     ssSaslPassword  "sasl-password"
       _ssHostName       <- fieldReq' ssHostName      (sectionOptString "hostname")
       _ssPort           <- field'    ssPort          (sectionOptNum "port")
       _ssTls            <- fieldReq' ssTls           (boolean "tls")
       _ssTlsInsecure    <- fieldReq' ssTlsInsecure   (boolean "tls-insecure")
       _ssTlsClientCert  <- field'    ssTlsClientCert (sectionOptString "tls-client-cert")
       _ssTlsClientKey   <- field'    ssTlsClientKey  (sectionOptString "tls-client-key")
       _ssConnectCmds    <- fieldReq  ssConnectCmds   "connect-cmds"
       _ssSocksHost      <- field'    ssSocksHost     (sectionOptString "socks-host")
       _ssSocksPort      <- fieldReq' ssSocksPort     (sectionOptNum "socks-port")
       _ssServerCerts    <- fieldReq' ssServerCerts   (sectionOptStrings "server-certificates")
       return ServerSettings{..}
  where
    field    l key = field'    l (sectionOpt key)
    fieldReq l key = fieldReq' l (sectionOpt key)

    fieldReq' l p = fromMaybe (view l def) <$> p

    field' l p = (<|> view l def) <$> p

boolean :: Text -> SectionParser (Maybe Bool)
boolean key =
  do mb <- sectionOpt key
     for mb $ \a ->
       case atomName a of
         "yes" -> return True
         "no"  -> return False
         _     -> liftConfigParser (failure "expected yes or no")
