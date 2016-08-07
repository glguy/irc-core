{-# Language OverloadedStrings #-}

{-|
Module      : Client.Hook.Znc.Buffextras
Description : Hook to remap znc buffextras messages
Copyright   : (c) Dan Doel, 2016
License     : ISC
Maintainer  : dan.doel@gmail.com

This hook remaps output from the znc buffextras plugin to the
actual IRC commands they represent, so that they can show up
normally in the client output.

-}

module Client.Hook.Znc.Buffextras
  ( buffextrasHook
  ) where

import Control.Lens
import Data.Attoparsec.Text as P
import Data.Monoid
import Data.Text as T hiding (head)

import Client.Hook
import Irc.Identifier
import Irc.Message
import Irc.RawIrcMsg
import Irc.UserInfo

buffextrasHook :: Bool -> MessageHook
buffextrasHook = MessageHook "buffextras" False . remap

remap :: Bool -> IrcMsg -> MessageResult
remap debug (Privmsg user chan msg)
  | userNick user == mkId "*buffextras"
  , Right newMsg <- parseOnly (mainParser chan) msg
  = RemapMessage newMsg

  | userNick user == mkId "*buffextras"
  , not debug
  = OmitMessage

remap _ _ = PassMessage

-- Note: the "Server set mode:" message is intentionally not handled at this
-- time.
mainParser :: Identifier -> Parser IrcMsg
mainParser = prefixedParser

prefixedParser :: Identifier -> Parser IrcMsg
prefixedParser chan = do
    pfx <- prefixParser
    choice [ Join pfx chan <$ sepMsg "joined"
           , parseLeave "quit" (Quit pfx)
           , parseLeave "parted" (Part pfx chan)
           , sepMsg "is now known as" *> fmap (Nick pfx . mkId) simpleTokenParser
           ]
 where
 remap cmd parms = set msgCommand cmd . over msgParams (parms . head)

sepMsg :: Text -> Parser ()
sepMsg m = P.skipWhile (==' ') *> string m *> P.skipWhile (==' ')

-- Parts and quits have a similar format.
parseLeave
  :: Text
  -> (Maybe Text -> IrcMsg)
  -> Parser IrcMsg
parseLeave small cmd = do
  sepMsg (small <> " with message:")
  P.skipWhile (==' ')
  char '[' *> fmap (cmd . filterEmpty) (P.takeWhile (/=']')) <* char ']'

filterEmpty :: Text -> Maybe Text
filterEmpty tx
  | T.null tx = Nothing
  | otherwise = Just tx
