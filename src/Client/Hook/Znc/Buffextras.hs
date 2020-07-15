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
buffextrasHook :: [Text] {- ^ arguments -} -> Maybe MessageHook
buffextrasHook args =
  case args of
    []        -> Just (MessageHook "buffextras" False (remap False))
    ["debug"] -> Just (MessageHook "buffextras" False (remap True))
    _         -> Nothing

remap ::
  Bool {- ^ enable debugging -} ->
  IrcMsg -> MessageResult
remap debug (Privmsg user chan msg)
  | userNick user == "*buffextras"
  , Right newMsg <- parseOnly (prefixedParser chan) msg
  = RemapMessage newMsg

  | userNick user == "*buffextras"
  , not debug
  = OmitMessage

remap _ _ = PassMessage

prefixedParser :: Identifier -> Parser IrcMsg
prefixedParser chan = do
    pfx <- prefixParser
    choice
      [ Join pfx chan "" "" <$ skipToken "joined"
      , Quit pfx . filterEmpty <$ skipToken "quit:" <*> P.takeText
      , Part pfx chan . filterEmpty <$ skipToken "parted:" <*> P.takeText
      , Nick pfx . mkId <$ skipToken "is now known as" <*> simpleTokenParser
      , Mode pfx chan <$ skipToken "set mode:" <*> allTokens
      , Kick pfx chan <$ skipToken "kicked" <*> parseId <* skipToken "with reason:" <*> P.takeText
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
