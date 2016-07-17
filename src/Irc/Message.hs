{-# Language OverloadedStrings #-}
module Irc.Message where

import Irc.Identifier
import Irc.RawIrcMsg
import Irc.UserInfo
import Data.Text (Text)
import Data.Text.Read as Text
import Data.Text as Text
import Control.Lens

data IrcMsg
  = UnknownMsg RawIrcMsg
  | Reply Int [Text]
  | Nick Identifier Identifier
  | Join Identifier Identifier
  | Part Identifier Identifier
  | Quit Identifier Text
  | Kick Identifier Identifier Identifier Text
  | Topic Identifier Identifier Text
  | Privmsg Identifier Identifier Text
  | Notice Identifier Identifier Text
  | Mode Identifier Identifier [Text]
  | Cap CapCmd [Text]
  | Ping [Text]
  | Pong [Text]
  | Error Text
  deriving Show

data CapCmd
  = CapLs
  | CapList
  | CapReq
  | CapAck
  | CapNak
  deriving (Show, Eq, Ord)

isErrorCode :: Int -> Bool
isErrorCode x = 400 <= x && x < 600

cookCapCmd :: Text -> Maybe CapCmd
cookCapCmd "LS"   = Just CapLs
cookCapCmd "LIST" = Just CapList
cookCapCmd "REQ"  = Just CapReq
cookCapCmd "ACK"  = Just CapAck
cookCapCmd "NAK"  = Just CapNak
cookCapCmd _      = Nothing

cookIrcMsg :: RawIrcMsg -> IrcMsg
cookIrcMsg msg =
  case view msgCommand msg of
    cmd | Right (n,"") <- decimal cmd ->
        Reply n (view msgParams msg)
    "CAP" | _target:cmdTxt:rest <- view msgParams msg
          , Just cmd <- cookCapCmd cmdTxt ->
           Cap cmd rest

    "PING" -> Ping (view msgParams msg)
    "PONG" -> Pong (view msgParams msg)

    "PRIVMSG" | Just user <- view msgPrefix msg
           , [chan,txt]   <- view msgParams msg ->
           Privmsg (userNick user) (mkId chan) txt

    "NOTICE" | Just user <- view msgPrefix msg
           , [chan,txt]    <- view msgParams msg ->
           Notice (userNick user) (mkId chan) txt

    "JOIN" | Just user <- view msgPrefix msg
           , chan:_    <- view msgParams msg ->

           Join (userNick user) (mkId chan)

    "QUIT" | Just user <- view msgPrefix msg
           , [reason]  <- view msgParams msg ->
           Quit (userNick user) reason

    "PART" | Just user <- view msgPrefix msg
           , chan:_    <- view msgParams msg ->
           Part (userNick user) (mkId chan)

    "NICK"  | Just user <- view msgPrefix msg
            , newNick:_ <- view msgParams msg ->
           Nick (userNick user) (mkId newNick)

    "KICK"  | Just user <- view msgPrefix msg
            , [chan,nick,reason] <- view msgParams msg ->
           Kick (userNick user) (mkId chan) (mkId nick) reason

    "TOPIC" | Just user <- view msgPrefix msg
            , [chan,topic] <- view msgParams msg ->
            Topic (userNick user) (mkId chan) topic

    "MODE"  | Just user <- view msgPrefix msg
            , target:modes <- view msgParams msg ->
            Mode (userNick user) (mkId target) modes

    "ERROR" | [reason] <- view msgParams msg ->
            Error reason

    _      -> UnknownMsg msg

data MessageTarget
  = TargetUser Identifier
  | TargetWindow Identifier
  | TargetNetwork
  | TargetHidden

msgTarget :: Identifier -> IrcMsg -> MessageTarget
msgTarget me msg =
  case msg of
    UnknownMsg{} -> TargetNetwork
    Reply{} -> TargetNetwork
    Nick user _ -> TargetUser user
    Mode _ tgt _ | tgt == me -> TargetNetwork
                 | otherwise -> TargetWindow tgt
    Join _ chan -> TargetWindow chan
    Part _ chan -> TargetWindow chan
    Quit user _ -> TargetUser user
    Kick _ chan _ _ -> TargetWindow chan
    Topic _ chan _ -> TargetWindow chan
    Privmsg src tgt _ | tgt == me -> TargetWindow src
                      | otherwise -> TargetWindow tgt
    Notice  src tgt _ | tgt == me -> TargetWindow src
                      | otherwise -> TargetWindow tgt
    Ping{} -> TargetHidden
    Pong{} -> TargetNetwork
    Error{} -> TargetNetwork
    Cap{} -> TargetNetwork

msgActor :: IrcMsg -> Maybe Identifier
msgActor msg =
  case msg of
    UnknownMsg{}  -> Nothing
    Reply{}       -> Nothing
    Nick x _      -> Just x
    Join x _      -> Just x
    Part x _      -> Just x
    Quit x _      -> Just x
    Kick x _ _ _  -> Just x
    Topic x _ _   -> Just x
    Privmsg x _ _ -> Just x
    Notice x _ _  -> Just x
    Mode x _ _    -> Just x
    Ping{}        -> Nothing
    Pong{}        -> Nothing
    Error{}       -> Nothing
    Cap{}         -> Nothing

ircMsgText :: IrcMsg -> Text
ircMsgText msg =
  case msg of
    UnknownMsg raw ->
        Text.unwords (view msgCommand raw : view msgParams raw)
    Reply n xs -> Text.unwords (Text.pack (show n) : xs)
    Nick x y -> Text.unwords [idText x, idText y]
    Join x _ -> idText x
    Part x _ -> idText x
    Quit x y -> Text.unwords [idText x, y]
    Kick x _ z r -> Text.unwords [idText x, idText z, r]
    Topic x _ t -> Text.unwords [idText x, t]
    Privmsg x _ t -> Text.unwords [idText x, t]
    Notice x _ t -> Text.unwords [idText x, t]
    Mode x _ xs -> Text.unwords (idText x:xs)
    Ping xs -> Text.unwords xs
    Pong xs -> Text.unwords xs
    Cap _ xs -> Text.unwords xs
    Error t -> t
