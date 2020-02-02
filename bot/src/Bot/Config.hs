{-|
Module      : Bot.Config
Description : Configuration for bot
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Bot.Config where

import Control.Monad
import Data.Foldable
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Config = Config
  { configNick :: String
  , configHost :: String
  , configAdmins :: [String]
  , configSaslUser :: Maybe String
  , configSaslPass :: Maybe String
  }

-- | Get the hostname from the command-line arguments
getConfig :: IO Config
getConfig =
  do args <- getArgs
     case getOpt RequireOrder optDescrs args of

       (updateFuns, [nick, host], []) ->
         let config0 = Config nick host [] Nothing Nothing in
         case foldM (\acc f -> f acc) config0 updateFuns of
           Left e       -> optionFailure [e]
           Right config -> pure config

       (_, []     , _) -> optionFailure ["Missing NICK and HOST options"]
       (_, [_]    , _) -> optionFailure ["Missing HOST options"]
       (_, _:_:_:_, _) -> optionFailure ["Too many commandline arguments"]
       (_, _, errors)  -> optionFailure errors

optionFailure :: [String] -> IO a
optionFailure errors =
  do hPutStrLn stderr (usageInfo "bot <flags> NICK HOST" optDescrs)
     traverse_ (hPutStrLn stderr) errors
     exitFailure

type Flag = Config -> Either String Config

optDescrs :: [OptDescr Flag]
optDescrs =
  [ Option ['a'] ["admin"] (ReqArg addAdmin    "NICK") "Add an admin to the bot"
  , Option ['u'] ["user"]  (ReqArg setSaslUser "USER") "SASL username"
  , Option ['p'] ["pass"]  (ReqArg setSaslPass "PASS") "SASL password"
  ]

addAdmin :: String -> Flag
addAdmin nick config = Right config { configAdmins = nick : configAdmins config }

setSaslUser :: String -> Flag
setSaslUser user config = Right config { configSaslUser = Just user }

setSaslPass :: String -> Flag
setSaslPass pass config = Right config { configSaslPass = Just pass }
