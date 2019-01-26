{-# Language OverloadedStrings #-}

{-|
Module      : Irc.Message
Description : High-level representation of IRC messages
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module defines high-level IRC commands. Commands are interpreted
and their arguments are extracted into the appropriate types.

-}

module Irc.Message
  (
  -- * High-level messages
    IrcMsg(..)
  , CapCmd(..)
  , cookIrcMsg

  -- * Properties of messages
  , MessageTarget(..)
  , ircMsgText
  , msgTarget
  , msgActor

  -- * Helper functions
  , nickSplit
  , computeMaxMessageLength
  ) where

import           Control.Monad
import           Data.Function
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Read as Text
import           Irc.Identifier
import           Irc.RawIrcMsg
import           Irc.UserInfo
import           Irc.Codes
import           View

-- | High-level IRC message representation
data IrcMsg
  = UnknownMsg !RawIrcMsg -- ^ pass-through for unhandled messages
  | Reply !ReplyCode [Text] -- ^ code arguments
  | Nick !UserInfo !Identifier -- ^ old new
  | Join !UserInfo !Identifier !Text -- ^ user channel account(extended-join)
  | Part !UserInfo !Identifier (Maybe Text) -- ^ user channel reason
  | Quit !UserInfo (Maybe Text) -- ^ user reason
  | Kick !UserInfo !Identifier !Identifier !Text -- ^ kicker channel kickee comment
  | Topic !UserInfo !Identifier !Text -- ^ user channel topic
  | Privmsg !UserInfo !Identifier !Text -- ^ source target txt
  | Ctcp !UserInfo !Identifier !Text !Text -- ^ source target command txt
  | CtcpNotice !UserInfo !Identifier !Text !Text -- ^ source target command txt
  | Notice !UserInfo !Identifier !Text -- ^ source target txt
  | Mode !UserInfo !Identifier [Text] -- ^ source target txt
  | Authenticate !Text -- ^ parameters
  | Cap !CapCmd [Text] -- ^ command parameters
  | Ping [Text] -- ^ parameters
  | Pong [Text] -- ^ parameters
  | Error !Text -- ^ message
  | BatchStart !Text !Text [Text] -- ^ reference-id type parameters
  | BatchEnd !Text -- ^ reference-id
  | Account !UserInfo !Text -- ^ user account name changed (account-notify extension)
  | Chghost !UserInfo !Text !Text -- ^ Target, new username and new hostname
  deriving Show

-- | Sub-commands of the CAP command
data CapCmd
  = CapLs -- ^ request list of supported caps
  | CapList -- ^ request list of active caps
  | CapReq -- ^ request activation of cap
  | CapAck -- ^ request accepted
  | CapNak -- ^ request denied
  | CapEnd -- ^ end negotiation
  | CapNew -- ^ new capability available (cap-notify extension)
  | CapDel -- ^ capability removed (cap-notify extension)
  deriving (Show, Eq, Ord)

-- | Match command text to structured cap sub-command
cookCapCmd :: Text -> Maybe CapCmd
cookCapCmd "LS"   = Just CapLs
cookCapCmd "LIST" = Just CapList
cookCapCmd "ACK"  = Just CapAck
cookCapCmd "NAK"  = Just CapNak
cookCapCmd "END"  = Just CapEnd
cookCapCmd "REQ"  = Just CapReq
cookCapCmd "NEW"  = Just CapNew
cookCapCmd "DEL"  = Just CapDel
cookCapCmd _      = Nothing

-- | Interpret a low-level 'RawIrcMsg' as a high-level 'IrcMsg'.
-- Messages that can't be understood are wrapped in 'UnknownMsg'.
cookIrcMsg :: RawIrcMsg -> IrcMsg
cookIrcMsg msg =
  case view msgCommand msg of
    cmd | Right (n,"") <- decimal cmd ->
        Reply (ReplyCode n) (view msgParams msg)
    "CAP" | _target:cmdTxt:rest <- view msgParams msg
          , Just cmd <- cookCapCmd cmdTxt ->
           Cap cmd rest

    "AUTHENTICATE" | x:_ <- view msgParams msg ->
        Authenticate x

    "PING" -> Ping (view msgParams msg)
    "PONG" -> Pong (view msgParams msg)

    "PRIVMSG" | Just user <- view msgPrefix msg
           , [chan,txt]   <- view msgParams msg ->

           case parseCtcp txt of
             Just (cmd,args) -> Ctcp user (mkId chan) (Text.toUpper cmd) args
             Nothing         -> Privmsg user (mkId chan) txt

    "NOTICE" | Just user <- view msgPrefix msg
           , [chan,txt]    <- view msgParams msg ->

           case parseCtcp txt of
             Just (cmd,args) -> CtcpNotice user (mkId chan) (Text.toUpper cmd) args
             Nothing         -> Notice user (mkId chan) txt

    "JOIN" | Just user <- view msgPrefix msg
           , chan:rest <- view msgParams msg ->

           let acct = case rest of
                 ["*" , _real] -> ""
                 [acct, _real] -> acct
                 _             -> ""
           in Join user (mkId chan) acct

    "QUIT" | Just user <- view msgPrefix msg
           , reasons   <- view msgParams msg ->
           Quit user (listToMaybe reasons)

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

    "BATCH" | refid : ty : params <- view msgParams msg
            , Just ('+',refid') <- Text.uncons refid ->
            BatchStart refid' ty params

    "BATCH" | [refid] <- view msgParams msg
            , Just ('-',refid') <- Text.uncons refid ->
            BatchEnd refid'

    "ACCOUNT" | Just user <- view msgPrefix msg
              , [acct] <- view msgParams msg ->
      Account user (if acct == "*" then "" else acct)

    "CHGHOST" | Just user <- view msgPrefix msg
              , [newuser, newhost] <- view msgParams msg ->
      Chghost user newuser newhost

    _      -> UnknownMsg msg

-- | Parse a CTCP encoded message:
--
-- @\^ACOMMAND arguments\^A@
parseCtcp :: Text -> Maybe (Text, Text)
parseCtcp txt =
  do txt1 <- Text.stripSuffix "\^A" =<< Text.stripPrefix "\^A" txt
     let (cmd,args) = Text.break (==' ') txt1
     guard (not (Text.null cmd))
     return (cmd, Text.drop 1 args)


-- | Targets used to direct a message to a window for display
data MessageTarget
  = TargetUser   !Identifier -- ^ Metadata update for a user
  | TargetWindow !Identifier -- ^ Directed message to channel or from user
  | TargetNetwork            -- ^ Network-level message
  | TargetHidden             -- ^ Completely hidden message

-- | Target information for the window that could be appropriate to
-- display this message in.
msgTarget :: Identifier -> IrcMsg -> MessageTarget
msgTarget me msg =
  case msg of
    UnknownMsg{}             -> TargetNetwork
    Nick user _              -> TargetUser (userNick user)
    Mode _ tgt _ | tgt == me -> TargetNetwork
                 | otherwise -> TargetWindow tgt
    Join _ chan _            -> TargetWindow chan
    Part _ chan _            -> TargetWindow chan
    Quit user _              -> TargetUser (userNick user)
    Kick _ chan _ _          -> TargetWindow chan
    Topic _ chan _           -> TargetWindow chan
    Privmsg src tgt _        -> directed src tgt
    Ctcp src tgt _ _         -> directed src tgt
    CtcpNotice src tgt _ _   -> directed src tgt
    Notice  src tgt _        -> directed src tgt
    Authenticate{}           -> TargetHidden
    Ping{}                   -> TargetHidden
    Pong{}                   -> TargetHidden
    Error{}                  -> TargetNetwork
    Cap{}                    -> TargetNetwork
    Reply code args          -> replyTarget code args
    BatchStart{}             -> TargetHidden
    BatchEnd{}               -> TargetHidden
    Account user _           -> TargetUser (userNick user)
    Chghost user _ _         -> TargetUser (userNick user)
  where
    directed src tgt
      | Text.null (userHost src) = TargetNetwork -- server message
      | tgt == me = TargetWindow (userNick src)
      | otherwise = TargetWindow tgt
      where
        src' = userNick src

    replyTarget RPL_TOPIC    (_:chan:_)   = TargetWindow (mkId chan)
    replyTarget RPL_INVITING (_:_:chan:_) = TargetWindow (mkId chan)
    replyTarget _                _        = TargetNetwork

-- | 'UserInfo' of the user responsible for a message.
msgActor :: IrcMsg -> Maybe UserInfo
msgActor msg =
  case msg of
    UnknownMsg{}  -> Nothing
    Reply{}       -> Nothing
    Nick x _      -> Just x
    Join x _ _    -> Just x
    Part x _ _    -> Just x
    Quit x _      -> Just x
    Kick x _ _ _  -> Just x
    Topic x _ _   -> Just x
    Privmsg x _ _ -> Just x
    Ctcp x _ _ _  -> Just x
    CtcpNotice x _ _ _ -> Just x
    Notice x _ _  -> Just x
    Mode x _ _    -> Just x
    Account x _   -> Just x
    Authenticate{}-> Nothing
    Ping{}        -> Nothing
    Pong{}        -> Nothing
    Error{}       -> Nothing
    Cap{}         -> Nothing
    BatchStart{}  -> Nothing
    BatchEnd{}    -> Nothing

-- | Text representation of an IRC message to be used for matching with
-- regular expressions.
ircMsgText :: IrcMsg -> Text
ircMsgText msg =
  case msg of
    UnknownMsg raw -> Text.unwords (view msgCommand raw : view msgParams raw)
    Reply (ReplyCode n) xs -> Text.unwords (Text.pack (show n) : xs)
    Nick x y       -> Text.unwords [renderUserInfo x, idText y]
    Join x _ _     -> renderUserInfo x
    Part x _ mb    -> Text.unwords (renderUserInfo x : maybeToList mb)
    Quit x mb      -> Text.unwords (renderUserInfo x : maybeToList mb)
    Kick x _ z r   -> Text.unwords [renderUserInfo x, idText z, r]
    Topic x _ t    -> Text.unwords [renderUserInfo x, t]
    Privmsg x _ t  -> Text.unwords [renderUserInfo x, t]
    Ctcp x _ c t   -> Text.unwords [renderUserInfo x, c, t]
    CtcpNotice x _ c t -> Text.unwords [renderUserInfo x, c, t]
    Notice x _ t   -> Text.unwords [renderUserInfo x, t]
    Mode x _ xs    -> Text.unwords (renderUserInfo x:"set mode":xs)
    Ping xs        -> Text.unwords xs
    Pong xs        -> Text.unwords xs
    Cap _ xs       -> Text.unwords xs
    Error t        -> t
    Account x a    -> Text.unwords [renderUserInfo x, a]
    Authenticate{} -> ""
    BatchStart{}   -> ""
    BatchEnd{}     -> ""

-- nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
-- letter     =  %x41-5A / %x61-7A       ; A-Z / a-z
-- digit      =  %x30-39                 ; 0-9
-- special    =  %x5B-60 / %x7B-7D
isNickChar :: Char -> Bool
isNickChar x = '0' <= x && x <= '9'
              || 'A' <= x && x <= '}'
              || '-' == x

-- | Split a nick into text parts group by whether or not those parts are valid
-- nickname characters.
nickSplit :: Text -> [Text]
nickSplit = Text.groupBy ((==) `on` isNickChar)

-- | Maximum length computation for the message part for
-- privmsg and notice. Note that the need for the limit is because
-- the server will limit the length of the message sent out to each
-- client, not just the length of the messages it will recieve.
--
-- Note that the length is on the *encoded message* which is UTF-8
-- The calculation isn't using UTF-8 on the userinfo part because
-- I'm assuming that the channel name and userinfo are all ASCII
--
-- @
-- :my!user@info PRIVMSG #channel :messagebody\r\n
-- @
computeMaxMessageLength :: UserInfo -> Text -> Int
computeMaxMessageLength myUserInfo target
  = 512 -- max IRC command
  - Text.length (renderUserInfo myUserInfo)
  - length (": PRIVMSG  :\r\n"::String)
  - Text.length target
