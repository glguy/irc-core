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

import Config (Value(..), Section(..), parse, pretty)
import Config.Number (integerToNumber)
import Config.Schema.Load (loadValue)
import Config.Schema.Spec
import Control.Exception (try)
import Control.Lens (makeLenses)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (formatParseM, formatShow, ISO8601(iso8601Format))
import System.Directory (getXdgDirectory, XdgDirectory(XdgConfig), createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)

data StsPolicy = StsPolicy
  { _stsExpiration :: !UTCTime
  , _stsPort       :: !Int
  }
  deriving (Show)

type StsPolicies = HashMap Text StsPolicy

makeLenses ''StsPolicy

policySpec :: ValueSpec StsPolicies
policySpec = HashMap.fromList <$> listSpec policyEntry

policyEntry :: ValueSpec (Text, StsPolicy)
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
            (Number () (integerToNumber (fromIntegral (_stsPort v)))),
          Section () "until"
            (Text ()
               (Text.pack
                 (formatShow iso8601Format
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
     try (do createDirectoryIfMissing True (takeDirectory path)
             writeFile path (encodePolicy sts ++ "\n")) :: IO (Either IOError ())
     return ()

dateTimeSpec :: ValueSpec UTCTime
dateTimeSpec
  = customSpec "date-time" stringSpec
  $ maybe (Left "unable to parse") Right
  . formatParseM iso8601Format
