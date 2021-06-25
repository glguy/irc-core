{-# Language OverloadedStrings #-}
{-|
Module      : Client.Hook.Matterbridge
Description : Hook for intergrating Matterbridge bridged messages
Copyright   : (c) Felix Friedlander 2021
License     : ISC
Maintainer  : felix@ffetc.net

Matterbridge is a simple multi-protocol chat bridge, supporting
dozens of different protocols. This hook makes Matterbridged messages
appear native in the client.

message-hooks configuration takes the following form:

> ["matterbridge", "nick", "#chan1", "#chan2", ..., "#chann"]

This hook assumes the Matterbridge RemoteNickFormat is simply
"<{NICK}> ".

-}
module Client.Hook.Matterbridge (matterbridgeHook) where

import Data.Text (Text)

import Control.Lens (set, view)

import Text.Regex.TDFA ((=~))

import Client.Hook (MessageHook(..), MessageResult(..))
import Irc.Message
import Irc.Identifier (mkId, Identifier)
import Irc.UserInfo (UserInfo(..), uiNick)

data MbMsg = Msg | Act

matterbridgeHook :: [Text] -> Maybe MessageHook
matterbridgeHook (nick:chans) = Just $ MessageHook "matterbridge" False $ remap (mkId nick) (map mkId chans)
matterbridgeHook _ = Nothing

remap :: Identifier -> [Identifier] -> IrcMsg -> MessageResult
remap nick chans ircmsg = case ircmsg of
  (Privmsg (Source ui _) chan msg)
    | view uiNick ui == nick, chan `elem` chans -> remap' Msg ui chan msg
  (Ctcp (Source ui _) chan "ACTION" msg)
    | view uiNick ui == nick, chan `elem` chans -> remap' Act ui chan msg
  _ -> PassMessage

remap' :: MbMsg -> UserInfo -> Identifier -> Text -> MessageResult
remap' mbmsg ui chan msg
  | (_,_,_,[nick,msg']) <- msg =~ ("^<([^>]+)> (.*)$" :: Text) :: (Text, Text, Text, [Text])
  = RemapMessage $ newmsg mbmsg (fakeUser ui nick) chan msg'
  | otherwise = PassMessage

newmsg :: MbMsg -> Source -> Identifier -> Text -> IrcMsg
newmsg Msg src chan msg = Privmsg src chan msg
newmsg Act src chan msg = Ctcp src chan "ACTION" msg

fakeUser :: UserInfo -> Text -> Source
fakeUser ui nick = Source (set uiNick (mkId nick) ui) ""
