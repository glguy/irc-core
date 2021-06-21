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
  , CapMore(..)
  , cookIrcMsg

  -- * Properties of messages
  , MessageTarget(..)
  , ircMsgText
  , msgTarget
  , msgActor
  , msgSource

  -- * Helper functions
  , nickSplit
  , computeMaxMessageLength
  , capCmdText

  , Source(..)
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
  | Reply !Text !ReplyCode [Text] -- ^ server code arguments
  | Nick !Source !Identifier -- ^ old new
  | Join !Source !Identifier !Text !Text -- ^ user channel account account gecos
  | Part !Source !Identifier (Maybe Text) -- ^ user channel reason
  | Quit !Source (Maybe Text) -- ^ user reason
  | Kick !Source !Identifier !Identifier !Text -- ^ kicker channel kickee comment
  | Kill !Source !Identifier !Text -- ^ killer killee reason
  | Topic !Source !Identifier !Text -- ^ user channel topic
  | Privmsg !Source !Identifier !Text -- ^ source target txt
  | Ctcp !Source !Identifier !Text !Text -- ^ source target command txt
  | CtcpNotice !Source !Identifier !Text !Text -- ^ source target command txt
  | Notice !Source !Identifier !Text -- ^ source target txt
  | Mode !Source !Identifier [Text] -- ^ source target txt
  | Authenticate !Text -- ^ parameters
  | Cap !CapCmd -- ^ cap command and parameters
  | Ping [Text] -- ^ parameters
  | Pong [Text] -- ^ parameters
  | Error !Text -- ^ message
  | BatchStart !Text !Text [Text] -- ^ reference-id type parameters
  | BatchEnd !Text -- ^ reference-id
  | Account !Source !Text -- ^ user account name changed (account-notify extension)
  | Chghost !Source !Text !Text -- ^ Target, new username and new hostname
  | Wallops  !Source !Text -- ^ Braodcast message: Source, message
  | Invite !Source !Identifier !Identifier -- ^ sender target channel
  deriving Show

data Source = Source { srcUser :: {-# UNPACK #-}!UserInfo, srcAcct :: !Text }
  deriving Show

data CapMore = CapMore | CapDone
  deriving (Show, Read, Eq, Ord)

-- | Sub-commands of the CAP command sent by server
data CapCmd
  = CapLs !CapMore [(Text, Maybe Text)] -- ^ list of supported caps
  | CapList [Text] -- ^ list of active caps
  | CapAck [Text] -- ^ request accepted
  | CapNak [Text] -- ^ request denied
  | CapNew [(Text, Maybe Text)] -- ^ new capability available (cap-notify extension)
  | CapDel [Text] -- ^ capability removed (cap-notify extension)
  deriving (Show, Read, Eq, Ord)

-- | Match command text to structured cap sub-command
cookCapCmd :: Text -> [Text] -> Maybe CapCmd
cookCapCmd cmd args =
  case (cmd, args) of
    ("LS"  , ["*", caps]) -> Just (CapLs CapMore (splitCapList caps))
    ("LS"  , [     caps]) -> Just (CapLs CapDone (splitCapList caps))
    ("LIST", [     caps]) -> Just (CapList (Text.words caps))
    ("ACK" , [     caps]) -> Just (CapAck (Text.words caps))
    ("NAK" , [     caps]) -> Just (CapNak (Text.words caps))
    ("NEW" , [     caps]) -> Just (CapNew (splitCapList caps))
    ("DEL" , [     caps]) -> Just (CapDel (Text.words caps))
    _                     -> Nothing

msgSource :: RawIrcMsg -> Maybe Source
msgSource msg =
  case view msgPrefix msg of
    Nothing -> Nothing
    Just p ->
      case [a | TagEntry "account" a <- view msgTags msg ] of
        []  -> Just (Source p "")
        a:_ -> Just (Source p a)


-- | Interpret a low-level 'RawIrcMsg' as a high-level 'IrcMsg'.
-- Messages that can't be understood are wrapped in 'UnknownMsg'.
cookIrcMsg :: RawIrcMsg -> IrcMsg
cookIrcMsg msg =
  case view msgCommand msg of
    cmd | Just user <- view msgPrefix msg
        , Right (n,"") <- decimal cmd ->
        Reply (idText (userNick user)) (ReplyCode n) (view msgParams msg)
    "CAP" | _target:cmdTxt:rest <- view msgParams msg
          , Just cmd <- cookCapCmd cmdTxt rest -> Cap cmd

    "AUTHENTICATE" | x:_ <- view msgParams msg ->
        Authenticate x

    "PING" -> Ping (view msgParams msg)
    "PONG" -> Pong (view msgParams msg)

    "PRIVMSG" | Just source <- msgSource msg
           , [chan,txt]   <- view msgParams msg ->

           case parseCtcp txt of
             Just (cmd,args) -> Ctcp source (mkId chan) (Text.toUpper cmd) args
             Nothing         -> Privmsg source (mkId chan) txt

    "NOTICE" | Just source <- msgSource msg
           , [chan,txt]    <- view msgParams msg ->

           case parseCtcp txt of
             Just (cmd,args) -> CtcpNotice source (mkId chan) (Text.toUpper cmd) args
             Nothing         -> Notice source (mkId chan) txt

    "JOIN" | Just source <- msgSource msg
           , chan:rest <- view msgParams msg
           , let (a, r) = case rest of
                            [acct, real] -> (acct, real)
                            _            -> ("", "") ->
           Join source (mkId chan) a r

    "QUIT" | Just source <- msgSource msg
           , reasons   <- view msgParams msg ->
           Quit source (listToMaybe reasons)

    "PART" | Just source <- msgSource msg
           , chan:reasons <- view msgParams msg ->
           Part source (mkId chan) (listToMaybe reasons)

    "NICK"  | Just source <- msgSource msg
            , newNick:_ <- view msgParams msg ->
           Nick source (mkId newNick)

    "KICK"  | Just source <- msgSource msg
            , [chan,nick,reason] <- view msgParams msg ->
           Kick source (mkId chan) (mkId nick) reason

    "KILL"  | Just source <- msgSource msg
            , [nick,reason] <- view msgParams msg ->
           Kill source (mkId nick) reason

    "TOPIC" | Just source <- msgSource msg
            , [chan,topic] <- view msgParams msg ->
            Topic source (mkId chan) topic

    "MODE"  | Just source <- msgSource msg
            , target:modes <- view msgParams msg ->
            Mode source (mkId target) modes

    "ERROR" | [reason] <- view msgParams msg ->
            Error reason

    "BATCH" | refid : ty : params <- view msgParams msg
            , Just ('+',refid') <- Text.uncons refid ->
            BatchStart refid' ty params

    "BATCH" | [refid] <- view msgParams msg
            , Just ('-',refid') <- Text.uncons refid ->
            BatchEnd refid'

    "ACCOUNT" | Just source <- msgSource msg
              , [acct] <- view msgParams msg ->
      Account source (if acct == "*" then "" else acct)

    "CHGHOST" | Just source <- msgSource msg
              , [newuser, newhost] <- view msgParams msg ->
      Chghost source newuser newhost

    "WALLOPS" | Just source <- msgSource msg
              , [txt] <- view msgParams msg ->
      Wallops source txt

    "INVITE" | Just source <- msgSource msg
             , [target, channel] <- view msgParams msg ->
      Invite source (mkId target) (mkId channel)

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
  deriving (Show)

-- | Target information for the window that could be appropriate to
-- display this message in.
msgTarget :: Identifier -> IrcMsg -> MessageTarget
msgTarget me msg =
  case msg of
    UnknownMsg{}             -> TargetNetwork
    Nick user _              -> TargetUser (userNick (srcUser user))
    Mode _ tgt _ | tgt == me -> TargetNetwork
                 | otherwise -> TargetWindow tgt
    Join _ chan _ _          -> TargetWindow chan
    Part _ chan _            -> TargetWindow chan
    Quit user _              -> TargetUser (userNick (srcUser user))
    Kick _ chan _ _          -> TargetWindow chan
    Kill _ _ _               -> TargetNetwork
    Topic _ chan _           -> TargetWindow chan
    Invite{}                 -> TargetNetwork
    Privmsg src tgt _        -> directed (srcUser src) tgt
    Ctcp src tgt _ _         -> directed (srcUser src) tgt
    CtcpNotice src tgt _ _   -> directed (srcUser src) tgt
    Notice  src tgt _        -> directed (srcUser src) tgt
    Authenticate{}           -> TargetNetwork
    Ping{}                   -> TargetNetwork
    Pong{}                   -> TargetNetwork
    Error{}                  -> TargetNetwork
    Cap{}                    -> TargetNetwork
    Reply _ code args        -> replyTarget code args
    BatchStart{}             -> TargetNetwork
    BatchEnd{}               -> TargetNetwork
    Account user _           -> TargetUser (userNick (srcUser user))
    Chghost user _ _         -> TargetUser (userNick (srcUser user))
    Wallops _ _              -> TargetNetwork
  where
    directed src tgt
      | Text.null (userHost src) = TargetNetwork -- server message
      | tgt == me = TargetWindow (userNick src)
      | otherwise = TargetWindow tgt

    replyTarget RPL_TOPIC    (_:chan:_)   = TargetWindow (mkId chan)
    replyTarget RPL_INVITING (_:_:chan:_) = TargetWindow (mkId chan)
    replyTarget _                _        = TargetNetwork

-- | 'UserInfo' of the user responsible for a message.
msgActor :: IrcMsg -> Maybe Source
msgActor msg =
  case msg of
    UnknownMsg{}  -> Nothing
    Reply{}       -> Nothing
    Nick x _      -> Just x
    Join x _ _ _  -> Just x
    Part x _ _    -> Just x
    Quit x _      -> Just x
    Kick x _ _ _  -> Just x
    Kill x _ _    -> Just x
    Topic x _ _   -> Just x
    Privmsg x _ _ -> Just x
    Invite x _ _  -> Just x
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
    Chghost x _ _ -> Just x
    Wallops x _   -> Just x

renderSource :: Source -> Text
renderSource (Source u "") = renderUserInfo u
renderSource (Source u a) = renderUserInfo u <> "(" <> a <> ")"

-- | Text representation of an IRC message to be used for matching with
-- regular expressions.
ircMsgText :: IrcMsg -> Text
ircMsgText msg =
  case msg of
    UnknownMsg raw -> Text.unwords (view msgCommand raw : view msgParams raw)
    Reply srv (ReplyCode n) xs -> Text.unwords (srv : Text.pack (show n) : xs)
    Nick x y       -> Text.unwords [renderSource x, idText y]
    Join x _ _ _   -> renderSource x
    Part x _ mb    -> Text.unwords (renderSource x : maybeToList mb)
    Quit x mb      -> Text.unwords (renderSource x : maybeToList mb)
    Kick x _ z r   -> Text.unwords [renderSource x, idText z, r]
    Kill x z r     -> Text.unwords [renderSource x, idText z, r]
    Topic x _ t    -> Text.unwords [renderSource x, t]
    Privmsg x _ t  -> Text.unwords [renderSource x, t]
    Ctcp x _ c t   -> Text.unwords [renderSource x, c, t]
    CtcpNotice x _ c t -> Text.unwords [renderSource x, c, t]
    Notice x _ t   -> Text.unwords [renderSource x, t]
    Mode x _ xs    -> Text.unwords (renderSource x:"set mode":xs)
    Ping xs        -> Text.unwords xs
    Pong xs        -> Text.unwords xs
    Cap cmd        -> capCmdText cmd
    Error t        -> t
    Account x a    -> Text.unwords [renderSource x, a]
    Authenticate{} -> ""
    BatchStart{}   -> ""
    BatchEnd{}     -> ""
    Invite _ _ _   -> ""
    Chghost x a b  -> Text.unwords [renderSource x, a, b]
    Wallops x t    -> Text.unwords [renderSource x, t]

capCmdText :: CapCmd -> Text
capCmdText cmd =
  case cmd of
    CapLs more caps -> capMoreText more <> capUnsplitCaps caps
    CapNew     caps -> capUnsplitCaps caps
    CapList    caps -> Text.unwords caps
    CapAck     caps -> Text.unwords caps
    CapNak     caps -> Text.unwords caps
    CapDel     caps -> Text.unwords caps

capMoreText :: CapMore -> Text
capMoreText CapDone = ""
capMoreText CapMore = "* "

capUnsplitCaps :: [(Text, Maybe Text)] -> Text
capUnsplitCaps xs = Text.unwords [ k <> maybe "" ("=" <>) v | (k, v) <- xs ]

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

splitCapList :: Text -> [(Text, Maybe Text)]
splitCapList caps =
  [ (name, value)
    | kv <- Text.words caps
    , let (name, v) = Text.break ('=' ==) kv
    , let value | Text.null v = Nothing
                | otherwise   = Just $! Text.tail v
    ]
