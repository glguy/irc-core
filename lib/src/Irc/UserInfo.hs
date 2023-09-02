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
  (
  -- * Type
    UserInfo(..)

  -- * Parser and printer
  , renderUserInfo
  , parseUserInfo

  -- * Lenses
  , uiNick
  , uiName
  , uiHost
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Irc.Identifier

-- | 'UserInfo' packages a nickname along with the username and hostname
-- if they are known in the current context.
data UserInfo = UserInfo
  { userNick :: {-# UNPACK #-} !Identifier   -- ^ nickname
  , userName :: {-# UNPACK #-} !Text -- ^ username, empty when missing
  , userHost :: {-# UNPACK #-} !Text -- ^ hostname, empty when missing
  }
  deriving (Eq, Ord, Read, Show)

-- | Lens into 'userNick' field.
uiNick :: Functor f => (Identifier -> f Identifier) -> UserInfo -> f UserInfo
uiNick f ui@UserInfo{userNick = n} = (\n' -> ui{userNick = n'}) <$> f n

-- | Lens into 'userName' field.
uiName :: Functor f => (Text -> f Text) -> UserInfo -> f UserInfo
uiName f ui@UserInfo{userName = n} = (\n' -> ui{userName = n'}) <$> f n

-- | Lens into 'userHost' field.
uiHost :: Functor f => (Text -> f Text) -> UserInfo -> f UserInfo
uiHost f ui@UserInfo{userHost = n} = (\n' -> ui{userHost = n'}) <$> f n

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
