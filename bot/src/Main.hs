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
import           Data.Traversable (for)
import qualified Data.Text as Text
import           Irc.Codes
import           Irc.Commands
import           Irc.Identifier (Identifier, idText)
import           Irc.Message    (IrcMsg(Ping, Privmsg, Reply), cookIrcMsg)
import           Irc.RateLimit  (RateLimit, newRateLimit, tickRateLimit)
import           Irc.RawIrcMsg  (RawIrcMsg, parseRawIrcMsg, renderRawIrcMsg, asUtf8)
import           Irc.UserInfo   (userNick)
import           Hookup
import           System.Environment
import           System.Random

import           Data.HashMap.Strict (HashMap)
import           Data.HashSet (HashSet)
import           Data.IntMap (IntMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap

import           Data.Text (Text)
import qualified Data.Text as Text

import           Data.Machine
import           Control.Monad.IO.Class
import           Control.Arrow (Kleisli(..))
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class
import           Data.Coerce
import           Data.Foldable

data Config = Config
  { configNick :: String
  , configHost :: String
  , configRate :: RateLimit
  }

main :: IO ()
main =
  do config <- loadConfig
     withConnection config $ \h ->
        eventLoop config h

-- | Get the hostname from the command-line arguments
loadConfig :: IO Config
loadConfig =
  do args <- getArgs
     rate <- newRateLimit 2 8 -- safe defaults
     case args of
       [n,h] -> return (Config n h rate)
       _   -> fail "Usage: ./bot NICK HOSTNAME"

-- | Construct the connection parameters needed for the connection package
mkParams :: Config -> ConnectionParams
mkParams config = ConnectionParams
  { cpHost = configHost config
  , cpPort = 6667
  , cpTls = Nothing
  , cpSocks = Nothing
  }

-- | Open a connection which will stay open for duration of executing
-- the action returned by the continuation.
withConnection :: Config -> (Connection -> IO a) -> IO a
withConnection config k =
  do bracket (connect (mkParams config)) close k

-- | IRC specifies that messages will bit up to 512 bytes including the newline
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
         Just msg -> return $! cookIrcMsg msg
         Nothing  -> fail "Server sent invalid message!"

-- | Write an encoded IRC message to the connection
sendMsg :: Config -> Connection -> RawIrcMsg -> IO ()
sendMsg c h msg =
  do tickRateLimit (configRate c)
     send h (renderRawIrcMsg msg)

-- | Send initial @USER@ and @NICK@ messages
startup :: IrcPlan ()
startup =
  do config <- getConfig
     let botNick = Text.pack (configNick config)
         botUser = botNick
         botReal = botNick
         mode_w  = False
         mode_i  = True
     yield (ircUser botUser mode_w mode_i botReal)
     yield (ircNick botNick)

eventLoop :: Config -> Connection -> IO ()
eventLoop config h =
  flip evalStateT (BotState config mempty defaultBehaviors) $
  runT_ $
     starve (construct startup)
            (construct (exhaust (liftIO (readIrcLine h))) ~> repeatedly logic) ~>
     autoT (Kleisli (liftIO . sendMsg config h))

defaultBehaviors :: IntMap ([Text] -> IrcPlan ())
defaultBehaviors = IntMap.fromList
  $ map (\(ReplyCode x,y) -> (fromIntegral x, y))
  [ (RPL_WELCOME, \_ -> yield (ircJoin "#glguy" Nothing) )
  , (ERR_NICKNAMEINUSE, \_ ->
         do n <- liftIO (randomRIO (1,1000::Int))
            config <- getConfig
            let newNick = Text.pack (configNick config ++ show n)
            yield (ircNick newNick))
  , (RPL_NAMREPLY, nameReply [])
  ]

nameReply :: [Identifier] -> [Text] -> IrcPlan ()
nameReply acc args =
  case args of
    [_me,chan,_,names] -> Text.words names
    _ -> liftIO (putStrLn ("Unsupported name reply: " ++ show args))

type IrcPlan = PlanT (Is IrcMsg) RawIrcMsg (StateT BotState IO)

data BotState = BotState
  { bsConfig :: Config
  , bsChannels :: HashMap Identifier (HashSet Identifier)
  , bsBehavior :: IntMap ([Text] -> IrcPlan ())
  }

getConfig :: IrcPlan Config
getConfig = lift (gets bsConfig)

logic :: IrcPlan ()
logic =
  do msg <- await
     case msg of
       -- respond to pings
       Ping xs -> yield (ircPong xs)

       -- quit on request or echo message back as notices
       Privmsg who _me txt
         | txt == "!quit" -> yield (ircQuit "Quit requested")
         | otherwise      -> yield (ircNotice (idText (userNick who)) txt)

       Reply (ReplyCode code) args ->
         do mb <- lift (gets (IntMap.lookup (fromIntegral code) . bsBehavior))
            for_ mb $ \k -> k args

       _ -> liftIO (print msg)
