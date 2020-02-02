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
import           Data.Foldable (traverse_)
import           Data.Traversable (for)
import           Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Set as Set
import           Data.Set (Set)
-- import           Data.Text (Text)
import           Irc.Codes
import           Irc.Commands
import           Irc.Identifier
import           Irc.Message
import           Irc.RateLimit  (RateLimit, newRateLimit, tickRateLimit)
import           Irc.RawIrcMsg  (RawIrcMsg, parseRawIrcMsg, renderRawIrcMsg, asUtf8)
import           Irc.UserInfo
import           Hookup
import           System.Random

import           Bot.Config

data Bot = Bot
  { botConfig     :: Config
  , botRate       :: RateLimit
  , botConnection :: Connection
  , botNick       :: Identifier
  , botAdmins     :: Set Identifier
  }

main :: IO ()
main =
  do config <- getConfig
     withConnection config $ \h ->
        do rate <- newRateLimit 2 8
           let bot = Bot {
                 botConfig     = config,
                 botRate       = rate,
                 botConnection = h,
                 botNick       = "",
                 botAdmins     = Set.fromList (map fromString (configAdmins config))}
           eventLoop =<< sendHello bot

-- | Construct the connection parameters needed for the connection package
mkParams :: Config -> ConnectionParams
mkParams config = ConnectionParams
  { cpHost = configHost config
  , cpPort = 6697 -- IRC over TLS
  , cpTls = Just TlsParams
               { tpClientCertificate = Nothing
               , tpClientPrivateKey  = Nothing
               , tpServerCertificate = Nothing
               , tpCipherSuite       = "HIGH"
               , tpInsecure          = False }
  , cpSocks = Nothing
  , cpFamily = defaultFamily
  }

-- | Open a connection which will stay open for duration of executing
-- the action returned by the continuation.
withConnection :: Config -> (Connection -> IO a) -> IO a
withConnection config = bracket (connect (mkParams config)) close

-- | IRC specifies that messages will be up to 512 bytes including the newline
maxIrcMessage :: Int
maxIrcMessage = 512


-- | Read the next high-level IRC message off the connection. An empty message
-- is indicated by returning 'Nothing' and indicates that the connection is
-- finished.
readIrcLine :: Connection -> IO (Maybe IrcMsg)
readIrcLine h =
  do mb <- recvLine h maxIrcMessage
     for mb $ \xs ->
       case parseRawIrcMsg (asUtf8 xs) of
         Just msg -> pure $! cookIrcMsg msg
         Nothing  -> fail "Server sent invalid message!"

-- | Write an encoded IRC message to the connection
sendMsg :: Bot -> RawIrcMsg -> IO ()
sendMsg bot msg =
  do tickRateLimit (botRate bot)
     send (botConnection bot) (renderRawIrcMsg msg)

-- | Send initial @USER@ and @NICK@ messages
sendHello :: Bot -> IO Bot
sendHello bot =
  do let config  = botConfig bot
         nick = Text.pack (configNick config)
         user = nick
         real = nick

     case (configSaslUser config, configSaslPass config) of
       (Just u, Just p) ->
         do sendMsg bot (ircCapReq ["sasl"])
            authenticateLoop u p bot
       _ -> pure ()

     sendMsg bot (ircUser user real)
     sendMsg bot (ircNick nick)

     pure bot { botNick = mkId nick }

authenticateLoop :: String -> String -> Bot -> IO ()
authenticateLoop user pass bot =
  do mb <- readIrcLine (botConnection bot)
     msg <- case mb of
              Nothing -> fail "authenticateLoop: connection failed"
              Just x  -> pure x
     case msg of
       Cap (CapNak ["sasl"]) -> fail "sasl not supported"

       Cap (CapAck ["sasl"]) ->
         do sendMsg bot (ircAuthenticate "PLAIN")
            authenticateLoop user pass bot

       Authenticate "+" ->
         do let payload = encodePlainAuthentication (Text.pack user) (Text.pack pass)
            traverse_ (sendMsg bot) (ircAuthenticates payload)
            authenticateLoop user pass bot

       Reply RPL_SASLSUCCESS _ ->
         do sendMsg bot ircCapEnd

       Reply RPL_SASLFAIL    _ -> fail "SASL failed"
       Reply RPL_SASLTOOLONG _ -> fail "SASL failed"
       Reply RPL_SASLABORTED _ -> fail "SASL failed"
       Reply RPL_SASLALREADY _ -> fail "SASL failed"
       Reply RPL_SASLMECHS   _ -> fail "SASL failed"

       _ -> print msg >> authenticateLoop user pass bot


eventLoop :: Bot -> IO ()
eventLoop bot =
  do mb <- readIrcLine (botConnection bot)
     case mb of
       Nothing -> pure ()
       Just msg ->
         do print msg
            eventLoop =<< processIrcMsg bot msg

processIrcMsg :: Bot -> IrcMsg -> IO Bot
processIrcMsg bot msg =
  case msg of
    -- pong for the ping
    Ping xs ->
      do sendMsg bot (ircPong xs)
         pure bot

    -- quit on request or echo message back as notices
    Privmsg who _me txt
      | txt == "!quit" ->
        needAdmin bot who $
          do sendMsg bot (ircQuit "Quit requested")
             pure bot

    -- pick a random new nickname
    Reply ERR_NICKNAMEINUSE _ ->
      do n <- randomRIO (1,1000::Int)
         let newNick = Text.pack (configNick (botConfig bot) ++ show n)
         sendMsg bot (ircNick newNick)
         pure bot { botNick = mkId newNick }

    Nick oldNick newNick
      -- the server changed our nickname
      | userNick oldNick == botNick bot ->
        do pure bot { botNick = newNick }

    -- unsupported message; ignore it
    _ -> pure bot

needAdmin :: Bot -> UserInfo -> IO Bot -> IO Bot
needAdmin bot who action
  | Set.member (userNick who) (botAdmins bot) = action
  | otherwise = pure bot
