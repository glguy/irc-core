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

import Data.Attoparsec.Text as P
import Data.Monoid
import Data.Text as Text hiding (head)

import Client.Hook
import Irc.Identifier
import Irc.Message
import Irc.RawIrcMsg
import Irc.UserInfo

-- | Map ZNC's buffextras messages to native client messages.
-- Set debugging to pass through buffextras messages that
-- the hook doesn't understand.
buffextrasHook :: Bool {- ^ enable debugging -} -> MessageHook
buffextrasHook = MessageHook "buffextras" False . remap

remap ::
  Bool {- ^ enable debugging -} ->
  IrcMsg -> MessageResult
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
    choice [ Join pfx chan   <$  sepMsg "joined"
           , Quit pfx        <$> parseLeave "quit"
           , Part pfx chan   <$> parseLeave "parted"
           , Nick pfx . mkId <$  sepMsg "is now known as" <*> simpleTokenParser
           , Mode pfx chan   <$  sepMsg "set mode:" <*> allTokens
           ]

allTokens :: Parser [Text]
allTokens = Text.words <$> P.takeText

sepMsg :: Text -> Parser ()
sepMsg m = P.skipWhile (==' ') *> string m *> P.skipWhile (==' ')

-- Parts and quits have a similar format.
parseLeave
  :: Text
  -> Parser (Maybe Text)
parseLeave small =
  do sepMsg (small <> " with message:")
     P.skipWhile (==' ')
     filterEmpty <$ char '[' <*> P.takeWhile (/=']') <* char ']'

filterEmpty :: Text -> Maybe Text
filterEmpty tx
  | Text.null tx = Nothing
  | otherwise = Just tx
