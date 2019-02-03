{-# LANGUAGE ApplicativeDo, TemplateHaskell, OverloadedStrings #-}

{-|
Module      : Client.Configuration.Sts
Description : STS policy configuration
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

https://ircv3.net/specs/extensions/sts.html

-}
module Client.Configuration.Sts
  ( StsPolicy(..)
  , stsExpiration
  , stsPort

  , readPolicyFile
  , savePolicyFile
  ) where

import           Config (Value(..), Section(..), parse, pretty)
import           Config.Schema.Spec
import           Config.Schema.Load (loadValue)
import           Control.Exception (try)
import           System.IO.Error (IOError)
import           Control.Lens (makeLenses)
import           Data.Maybe (fromMaybe)
import           Data.Time (UTCTime, defaultTimeLocale, formatTime, parseTimeM, iso8601DateFormat)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Directory (getXdgDirectory, XdgDirectory(XdgConfig))
import           System.FilePath (FilePath, (</>))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data StsPolicy = StsPolicy
  { _stsExpiration :: !UTCTime
  , _stsPort       :: !Int
  }
  deriving (Show)

type StsPolicies = HashMap Text StsPolicy

makeLenses ''StsPolicy

policySpec :: ValueSpecs StsPolicies
policySpec = HashMap.fromList <$> listSpec policyEntry

policyEntry :: ValueSpecs (Text, StsPolicy)
policyEntry =
  sectionsSpec "sts-policy" $
  do hostname   <- reqSection "host" "Hostname"
     expiration <- reqSection' "until" dateTimeSpec "Expiration date"
     port       <- reqSection "port" "Port number"
     return (hostname, StsPolicy expiration port)

encodePolicy :: StsPolicies -> String
encodePolicy p =
  show $ pretty $
  List ()
    [ Sections ()
        [ Section () "host"
            (Text () k),
          Section () "port"
            (Number () 10 (fromIntegral (_stsPort v))),
          Section () "until"
            (Text ()
               (Text.pack
                 (formatTime defaultTimeLocale dateTimeFormat
                   (_stsExpiration v))))
        ]
    | (k, v) <- HashMap.toList p ]

decodePolicy :: Text -> Maybe StsPolicies
decodePolicy txt =
  case parse txt of
    Left _ -> Nothing
    Right rawval ->
      case loadValue policySpec rawval of
        Left _ -> Nothing
        Right policy -> Just policy

getPolicyFilePath :: IO FilePath
getPolicyFilePath =
  do dir <- getXdgDirectory XdgConfig "glirc"
     return (dir </> "sts.cfg")

readPolicyFile :: IO StsPolicies
readPolicyFile =
  do path <- getPolicyFilePath
     res <- try (Text.readFile path) :: IO (Either IOError Text)
     return $! case res of
       Left {}   -> HashMap.empty
       Right txt -> fromMaybe HashMap.empty (decodePolicy txt)

savePolicyFile :: StsPolicies -> IO ()
savePolicyFile sts =
  do path <- getPolicyFilePath
     writeFile path (encodePolicy sts ++ "\n")

dateTimeSpec :: ValueSpecs UTCTime
dateTimeSpec
  = customSpec "date-time" stringSpec
  $ parseTimeM False defaultTimeLocale dateTimeFormat

dateTimeFormat :: String
dateTimeFormat = iso8601DateFormat (Just "%H:%M:%S")
