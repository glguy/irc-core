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

message-hooks configuration takes one of two forms;
to operate on all channels:

> ["matterbridge", "nick"]

or, to operate only on selected channels:

> ["matterbridge", "nick", "#chan1", "#chan2", ..., "#chann"]

This hook assumes the Matterbridge RemoteNickFormat is simply
"<{NICK}> ".

-}
module Client.Hook.Matterbridge (matterbridgeHook) where

import Client.Hook (MessageHook(..), MessageResult(..))
import Control.Lens (set, view)
import Data.Text (Text)
import Irc.Identifier (mkId, Identifier)
import Irc.Message (IrcMsg(Ctcp, Privmsg), Source(Source))
import Irc.UserInfo (UserInfo(..), uiNick)
import Text.Regex.TDFA ((=~))

data MbMsg = Msg | Act

matterbridgeHook :: [Text] -> Maybe MessageHook
matterbridgeHook [] = Nothing
matterbridgeHook (nick:chans) = Just (MessageHook "matterbridge" False (remap (mkId nick) chanfilter))
 where
  chanfilter
    | null chans = const True
    | otherwise  = (`elem` map mkId chans)

remap :: Identifier -> (Identifier -> Bool) -> IrcMsg -> MessageResult
remap nick chanfilter ircmsg =
  case ircmsg of
    Privmsg (Source ui _) chan msg
      | view uiNick ui == nick, chanfilter chan -> remap' Msg ui chan msg
    Ctcp (Source ui _) chan "ACTION" msg
      | view uiNick ui == nick, chanfilter chan -> remap' Act ui chan msg
    _ -> PassMessage

remap' :: MbMsg -> UserInfo -> Identifier -> Text -> MessageResult
remap' mbmsg ui chan msg =
  case msg =~ ("^(\x03\&[0-9]{2})?<([^>]+)> \x0f?(.*)$"::Text) of
    [_,_,nick,msg']:_ -> RemapMessage (newmsg mbmsg (fakeUser nick ui) chan msg')
    _                 -> PassMessage

newmsg :: MbMsg -> Source -> Identifier -> Text -> IrcMsg
newmsg Msg src chan msg = Privmsg src chan msg
newmsg Act src chan msg = Ctcp src chan "ACTION" msg

fakeUser :: Text -> UserInfo -> Source
fakeUser nick ui = Source (set uiNick (mkId nick) ui) ""
