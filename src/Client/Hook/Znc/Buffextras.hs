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

import           Control.Monad
import           Data.Attoparsec.Text as P
import           Data.Text as Text hiding (head)

import           Client.Hook
import           Irc.Identifier
import           Irc.Message
import           Irc.RawIrcMsg
import           Irc.UserInfo

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
  , Right newMsg <- parseOnly (prefixedParser chan) msg
  = RemapMessage newMsg

  | userNick user == mkId "*buffextras"
  , not debug
  = OmitMessage

remap _ _ = PassMessage

prefixedParser :: Identifier -> Parser IrcMsg
prefixedParser chan = do
    pfx <- prefixParser
    choice
      [ Join pfx chan <$ skipToken "joined"
      , Quit pfx . filterEmpty <$ skipToken "quit with message:" <*> parseReason
      , Part pfx chan . filterEmpty <$ skipToken "parted with message:" <*> parseReason
      , Nick pfx . mkId <$ skipToken "is now known as" <*> simpleTokenParser
      , Mode pfx chan <$ skipToken "set mode:" <*> allTokens
      , Kick pfx chan <$ skipToken "kicked" <*> parseId <* skipToken "Reason:" <*> parseReason
      , Topic pfx chan <$ skipToken "changed the topic to:" <*> P.takeText
      ]

allTokens :: Parser [Text]
allTokens = Text.words <$> P.takeText

skipToken :: Text -> Parser ()
skipToken m = string m *> P.skipWhile (==' ')

parseId :: Parser Identifier
parseId = mkId <$> simpleTokenParser

filterEmpty :: Text -> Maybe Text
filterEmpty txt
  | Text.null txt = Nothing
  | otherwise     = Just txt

parseReason :: Parser Text
parseReason =
  do txt <- char '[' *> P.takeText
     guard (not (Text.null txt) && Text.last txt == ']')
     return (Text.init txt)
