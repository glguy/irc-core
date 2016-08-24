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
import           Data.Foldable (for_)
import qualified Data.Text as Text
import           Irc.Codes
import           Irc.Commands   (ircUser, ircNick, ircPong, ircNotice, ircQuit)
import           Irc.Identifier (mkId)
import           Irc.Message    (IrcMsg(Ping, Privmsg, Reply), cookIrcMsg)
import           Irc.RateLimit  (RateLimit, newRateLimit, tickRateLimit)
import           Irc.RawIrcMsg  (RawIrcMsg, parseRawIrcMsg, renderRawIrcMsg, asUtf8)
import           Irc.UserInfo   (userNick)
import           Network.Connection
import           System.Environment
import           System.Random

data Config = Config
  { configNick :: String
  , configHost :: String
  , configRate :: RateLimit
  }

main :: IO ()
main =
  do config <- getConfig
     withConnection config $ \h ->
        do sendHello config h
           eventLoop config h

-- | Get the hostname from the command-line arguments
getConfig :: IO Config
getConfig =
  do args <- getArgs
     rate <- newRateLimit 2 8 -- safe defaults
     case args of
       [n,h] -> return (Config n h rate)
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
sendMsg :: Config -> Connection -> RawIrcMsg -> IO ()
sendMsg c h msg =
  do tickRateLimit (configRate c)
     connectionPut h (renderRawIrcMsg msg)

-- | Send initial @USER@ and @NICK@ messages
sendHello :: Config -> Connection -> IO ()
sendHello config h =
  do let botNick = Text.pack (configNick config)
         botUser = botNick
         botReal = botNick
         mode_w  = False
         mode_i  = True
     sendMsg config h (ircUser botUser mode_w mode_i botReal)
     sendMsg config h (ircNick (mkId botNick))

eventLoop :: Config -> Connection -> IO ()
eventLoop config h =
  do mb <- readIrcLine h
     for_ mb $ \msg ->
       do print msg
          case msg of
            -- respond to pings
            Ping xs -> sendMsg config h (ircPong xs)

            -- quit on request or echo message back as notices
            Privmsg who _me txt
              | txt == "!quit" -> sendMsg config h (ircQuit "Quit requested")
              | otherwise      -> sendMsg config h (ircNotice (userNick who) txt)

            Reply ERR_NICKNAMEINUSE _ ->
              do n <- randomRIO (1,1000::Int)
                 let newNick = mkId (Text.pack (configNick config ++ show n))
                 sendMsg config h (ircNick newNick)

            _ -> return ()

          eventLoop config h
