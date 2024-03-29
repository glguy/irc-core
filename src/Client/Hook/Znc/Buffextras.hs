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

import Client.Hook (MessageHook(MessageHook), MessageResult(..))
import Data.Attoparsec.Text as P
import Data.Text as Text (Text, null, words)
import Irc.Identifier (Identifier, mkId)
import Irc.Message (IrcMsg(Topic, Privmsg, Join, Quit, Part, Nick, Mode, Kick), Source(Source, srcUser))
import Irc.RawIrcMsg (prefixParser, simpleTokenParser)
import Irc.UserInfo (UserInfo(userNick))

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
  | userNick (srcUser user) == "*buffextras"
  , Right newMsg <- parseOnly (prefixedParser chan) msg
  = RemapMessage newMsg

  | userNick (srcUser user) == "*buffextras"
  , not debug
  = OmitMessage

remap _ _ = PassMessage

prefixedParser :: Identifier -> Parser IrcMsg
prefixedParser chan = do
    pfx <- prefixParser
    let src = Source pfx ""
    choice
      [ Join src chan "" "" <$ skipToken "joined"
      , Quit src . filterEmpty <$ skipToken "quit:" <*> P.takeText
      , Part src chan . filterEmpty <$ skipToken "parted:" <*> P.takeText
      , Nick src . mkId <$ skipToken "is now known as" <*> simpleTokenParser
      , Mode src chan <$ skipToken "set mode:" <*> allTokens
      , Kick src chan <$ skipToken "kicked" <*> parseId <* skipToken "with reason:" <*> P.takeText
      , Topic src chan <$ skipToken "changed the topic to:" <*> P.takeText
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
