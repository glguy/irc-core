{-# Language OverloadedStrings #-}
module Irc.Message where

import           Control.Lens
import           Data.Function
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Read as Text
import           Irc.Identifier
import           Irc.RawIrcMsg
import           Irc.UserInfo

data IrcMsg
  = UnknownMsg RawIrcMsg
  | Reply Int [Text]
  | Nick UserInfo Identifier
  | Join UserInfo Identifier
  | Part UserInfo Identifier (Maybe Text)
  | Quit UserInfo Text
  | Kick UserInfo Identifier Identifier Text -- ^ kicker channel kickee comment
  | Topic UserInfo Identifier Text -- ^ user channel topic
  | Privmsg UserInfo Identifier Text -- ^ source target txt
  | Action UserInfo Identifier Text -- ^ source target txt
  | Notice UserInfo Identifier Text -- ^ source target txt
  | Mode UserInfo Identifier [Text] -- ^ source target txt
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

           case Text.stripSuffix "\^A" =<< Text.stripPrefix "\^AACTION " txt of
             Just action -> Action  user (mkId chan) action
             Nothing     -> Privmsg user (mkId chan) txt

    "NOTICE" | Just user <- view msgPrefix msg
           , [chan,txt]    <- view msgParams msg ->
           Notice user (mkId chan) txt

    "JOIN" | Just user <- view msgPrefix msg
           , chan:_    <- view msgParams msg ->

           Join user (mkId chan)

    "QUIT" | Just user <- view msgPrefix msg
           , [reason]  <- view msgParams msg ->
           Quit user reason

    "PART" | Just user    <- view msgPrefix msg
           , chan:reasons <- view msgParams msg ->
           Part user (mkId chan) (listToMaybe reasons)

    "NICK"  | Just user <- view msgPrefix msg
            , newNick:_ <- view msgParams msg ->
           Nick user (mkId newNick)

    "KICK"  | Just user <- view msgPrefix msg
            , [chan,nick,reason] <- view msgParams msg ->
           Kick user (mkId chan) (mkId nick) reason

    "TOPIC" | Just user <- view msgPrefix msg
            , [chan,topic] <- view msgParams msg ->
            Topic user (mkId chan) topic

    "MODE"  | Just user <- view msgPrefix msg
            , target:modes <- view msgParams msg ->
            Mode user (mkId target) modes

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
    UnknownMsg{}                  -> TargetNetwork
    Nick user _                   -> TargetUser (userNick user)
    Mode _ tgt _ | tgt == me      -> TargetNetwork
                 | otherwise      -> TargetWindow tgt
    Join _ chan                   -> TargetWindow chan
    Part _ chan _                 -> TargetWindow chan
    Quit user _                   -> TargetUser (userNick user)
    Kick _ chan _ _               -> TargetWindow chan
    Topic _ chan _                -> TargetWindow chan
    Privmsg src tgt _ | tgt == me -> TargetWindow (userNick src)
                      | otherwise -> TargetWindow tgt
    Action  src tgt _ | tgt == me -> TargetWindow (userNick src)
                      | otherwise -> TargetWindow tgt
    Notice  src tgt _ | tgt == me -> TargetWindow (userNick src)
                      | otherwise -> TargetWindow tgt
    Ping{}                        -> TargetHidden
    Pong{}                        -> TargetNetwork
    Error{}                       -> TargetNetwork
    Cap{}                         -> TargetNetwork
    Reply {}                      -> TargetNetwork

msgActor :: IrcMsg -> Maybe UserInfo
msgActor msg =
  case msg of
    UnknownMsg{}  -> Nothing
    Reply{}       -> Nothing
    Nick x _      -> Just x
    Join x _      -> Just x
    Part x _ _    -> Just x
    Quit x _      -> Just x
    Kick x _ _ _  -> Just x
    Topic x _ _   -> Just x
    Privmsg x _ _ -> Just x
    Action x _ _  -> Just x
    Notice x _ _  -> Just x
    Mode x _ _    -> Just x
    Ping{}        -> Nothing
    Pong{}        -> Nothing
    Error{}       -> Nothing
    Cap{}         -> Nothing

ircMsgText :: IrcMsg -> Text
ircMsgText msg =
  case msg of
    UnknownMsg raw -> Text.unwords (view msgCommand raw : view msgParams raw)
    Reply n xs     -> Text.unwords (Text.pack (show n) : xs)
    Nick x y       -> Text.unwords [renderUserInfo x, idText y]
    Join x _       -> renderUserInfo x
    Part x _ mb    -> Text.unwords (renderUserInfo x : maybeToList mb)
    Quit x y       -> Text.unwords [renderUserInfo x, y]
    Kick x _ z r   -> Text.unwords [renderUserInfo x, idText z, r]
    Topic x _ t    -> Text.unwords [renderUserInfo x, t]
    Privmsg x _ t  -> Text.unwords [renderUserInfo x, t]
    Action x _ t   -> Text.unwords [renderUserInfo x, t]
    Notice x _ t   -> Text.unwords [renderUserInfo x, t]
    Mode x _ xs    -> Text.unwords (renderUserInfo x:"set mode":xs)
    Ping xs        -> Text.unwords xs
    Pong xs        -> Text.unwords xs
    Cap _ xs       -> Text.unwords xs
    Error t        -> t

-- nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
-- letter     =  %x41-5A / %x61-7A       ; A-Z / a-z
-- digit      =  %x30-39                 ; 0-9
-- special    =  %x5B-60 / %x7B-7D
isNickChar :: Char -> Bool
isNickChar x = '0' <= x && x <= '9'
              || 'A' <= x && x <= '}'
              || '-' == x

nickSplit :: Text -> [Text]
nickSplit = Text.groupBy ((==) `on` isNickChar)

-- Maximum length computation for the message part for
-- privmsg and notice. Note that the need for the limit is because
-- the server will limit the length of the message sent out to each
-- client, not just the length of the messages it will recieve.
--
-- Note that the length is on the *encoded message* which is UTF-8
-- The calculation isn't using UTF-8 on the userinfo part because
-- I'm assuming that the channel name and userinfo are all ASCII
--
-- :my!user@info PRIVMSG #channel :messagebody\r\n
computeMaxMessageLength :: UserInfo -> Text -> Int
computeMaxMessageLength myUserInfo target
  = 512 -- max IRC command
  - Text.length (renderUserInfo myUserInfo)
  - length (": PRIVMSG  :\r\n"::String)
  - Text.length target
