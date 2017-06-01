{-# Language OverloadedStrings, BangPatterns #-}
{-|
Module      : Client.Log
Description : Support for logging IRC traffic
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides provides logging functionality for IRC traffic.

-}
module Client.Log where

import           Client.Configuration
import           Client.Image.Message (cleanText)
import           Client.Message
import           Control.Exception
import           Control.Lens hiding ((<.>))
import           Data.Monoid
import           Data.Time
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import           Irc.Identifier
import           Irc.Message
import           Irc.UserInfo
import           System.Directory
import           System.FilePath


-- | Log entry queued in client to be written by the event loop
data LogLine = LogLine
  { logBaseDir :: FilePath -- ^ log directory from server settings
  , logDay     :: Day      -- ^ localtime day
  , logTarget  :: Text     -- ^ channel or nickname
  , logLine    :: L.Text   -- ^ formatted log message text
  }


-- | Write the given log entry to the filesystem.
writeLogLine ::
  LogLine  {- ^ log line -} ->
  IO ()
writeLogLine ll = ignoreProblems $
  do dir' <- resolveConfigurationPath
             (logBaseDir ll </> Text.unpack (logTarget ll))
     let file = dir' </> formatTime defaultTimeLocale "%F" (logDay ll) <.> "log"

     let recursiveFlag = True
     createDirectoryIfMissing recursiveFlag dir'
     L.appendFile file (logLine ll)


-- | Ignore all 'IOErrors'
ignoreProblems :: IO () -> IO ()
ignoreProblems m = () <$ (try m :: IO (Either IOError ()))


-- | Construct a 'LogLine' for the given 'ClientMessage' when appropriate.
-- Only chat messages result in a log line.
renderLogLine ::
  ClientMessage {- ^ message       -} ->
  FilePath      {- ^ log directory -} ->
  Identifier    {- ^ target        -} ->
  Maybe LogLine
renderLogLine !msg dir target =
  case view msgBody msg of
    NormalBody{} -> Nothing
    ErrorBody {} -> Nothing
    IrcBody irc ->
      case irc of
        Privmsg who _ txt ->
           success (L.fromChunks ["<", idText (userNick who), "> ", cleanText txt])
        Notice who _ txt ->
           success (L.fromChunks ["-", idText (userNick who), "- ", cleanText txt])
        Ctcp who _ "ACTION" txt ->
           success (L.fromChunks ["* ", idText (userNick who), " ", cleanText txt])
        _          -> Nothing

  where
    localtime = zonedTimeToLocalTime (view msgTime msg)
    day       = localDay localtime
    tod       = localTimeOfDay localtime
    todStr    = formatTime defaultTimeLocale "%T" tod

    success txt = Just LogLine
      { logBaseDir = dir
      , logDay     = day
      , logTarget  = Text.toLower (idTextNorm target)
      , logLine    = L.fromChunks ["[", Text.pack todStr, "] "] <> txt <> "\n"
      }
