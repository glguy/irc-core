{-# Language OverloadedStrings #-}

{-|
Module      : Irc.UserInfo
Description : User hostmasks
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

Information identifying users on IRC. This information includes
a nickname and optionally a username and hostname.

-}

module Irc.UserInfo
  ( UserInfo(..)
  , renderUserInfo
  , parseUserInfo
  , uiNick
  ) where

import           Data.Text      (Text)
import qualified Data.Text      as Text
import           Irc.Identifier
import           Data.Monoid ((<>))

-- | 'UserInfo' packages a nickname along with the username and hsotname
-- if they are known in the current context.
data UserInfo = UserInfo
  { userNick :: {-# UNPACK #-} !Identifier   -- ^ nickname
  , userName :: {-# UNPACK #-} !Text -- ^ username, empty when missing
  , userHost :: {-# UNPACK #-} !Text -- ^ hostname, empty when missing
  }
  deriving (Eq, Read, Show)

-- | 'Lens' into 'userNick' field.
uiNick :: Functor f => (Identifier -> f Identifier) -> UserInfo -> f UserInfo
uiNick f ui@UserInfo{userNick = n} = (\n' -> ui{userNick = n'}) <$> f n

-- | Render 'UserInfo' as @nick!username\@hostname@
renderUserInfo :: UserInfo -> Text
renderUserInfo (UserInfo a b c)
    = idText a
   <> (if Text.null b then "" else "!" <> b)
   <> (if Text.null c then "" else "@" <> c)

-- | Split up a hostmask into a nickname, username, and hostname.
-- The username and hostname might not be defined but are delimited by
-- a @!@ and @\@@ respectively.
parseUserInfo :: Text -> UserInfo
parseUserInfo x = UserInfo
  { userNick = mkId nick
  , userName = Text.drop 1 user
  , userHost = Text.drop 1 host
  }
  where
  (nickuser,host) = Text.break (=='@') x
  (nick,user) = Text.break (=='!') nickuser
