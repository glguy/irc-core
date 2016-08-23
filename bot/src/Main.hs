{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Simple echo bot
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides an example use of irc-core via an echo bot
-}
module Main (main) where

import           Control.Exception
import qualified Data.ByteString as B
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as Text
import           Irc.Commands
import           Irc.Identifier
import           Irc.Message
import           Irc.RawIrcMsg
import           Irc.UserInfo
import           Network.Connection
import           System.Environment

botNick, botUser, botReal :: Text
botNick = "irc-core-bot"
botUser = "irc-core-bot"
botReal = "irc-core-bot"

data Config = Config
  { configNick :: String
  , configHost :: String
  }

main :: IO ()
main =
  do config <- getConfig
     withConnection config $ \h ->
        do sendHello h config
           eventLoop h

-- | Get the hostname from the command-line arguments
getConfig :: IO Config
getConfig =
  do args <- getArgs
     case args of
       [n,h] -> return (Config n h)
       _   -> fail "Usage: ./bot NICK HOSTNAME"

-- | Construct the connection parameters needed for the connection package
mkParams :: Config -> ConnectionParams
mkParams config = ConnectionParams
  { connectionHostname  = configHost config
  , connectionPort      = 6697 -- IRC over TLS
  , connectionUseSecure = Just TLSSettingsSimple
      { settingDisableCertificateValidation = False
      , settingDisableSession               = False
      , settingUseServerName                = False
      }
  , connectionUseSocks = Nothing
  }

-- | Open a connection which will stay open for duration of executing
-- the action returned by the continuation.
withConnection :: Config -> (Connection -> IO a) -> IO a
withConnection config k =
  do ctx <- initConnectionContext
     bracket (connectTo ctx (mkParams config)) connectionClose k

-- | IRC specifies that messages will bit up to 512 bytes including the newline
maxIrcMessage :: Int
maxIrcMessage = 512

-- | Get a line from the connection. IRC terminates lines with @\r\n@
-- but connectionGetLine only checks for @\n@, so strip the @\r@
connectionGetLine' :: Connection -> IO B.ByteString
connectionGetLine' h =
  do xs <- connectionGetLine maxIrcMessage h
     return $! if B.null xs
                 then xs
                 else B.init xs -- drop '\r'

-- | Read the next high-level IRC message off the connection. An empty message
-- is indicated by returning 'Nothing' and indicates that the connection is
-- finished.
readIrcLine :: Connection -> IO (Maybe IrcMsg)
readIrcLine h =
  do xs <- connectionGetLine' h
     if B.null xs
       then return Nothing -- connection closed
       else case parseRawIrcMsg (asUtf8 xs) of
              Just msg -> return $! Just $! cookIrcMsg msg
              Nothing  -> fail "Server sent invalid message!"

-- | Write an encoded IRC message to the connection
sendMsg :: Connection -> RawIrcMsg -> IO ()
sendMsg h msg = connectionPut h (renderRawIrcMsg msg)

-- | Send initial @USER@ and @NICK@ messages
sendHello :: Connection -> Config -> IO ()
sendHello h config =
  do let botNick = Text.pack (configNick config)
         botUser = botNick
         botReal = botNick
         mode_w  = False
         mode_i  = True
     sendMsg h (ircUser botUser mode_w mode_i botReal)
     sendMsg h (ircNick (mkId botNick))

eventLoop :: Connection -> IO ()
eventLoop h =
  do mb <- readIrcLine h
     for_ mb $ \msg ->
       do print msg
          case msg of
            -- respond to pings
            Ping xs -> sendMsg h (ircPong xs)

            -- quit on request or echo message back as notices
            Privmsg who _me msg
              | msg == "!quit" -> sendMsg h (ircQuit "Quit requested")
              | otherwise      -> sendMsg h (ircNotice (userNick who) msg)

            _ -> return ()

          eventLoop h
