{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language TemplateHaskell #-}

{-|
Module      : Irc.RawIrcMsg
Description : Low-level representation of IRC messages
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a parser and printer for the low-level IRC
message format. It handles splitting up IRC commands into the
prefix, command, and arguments.

-}
module Irc.RawIrcMsg
  (
  -- * Low-level IRC messages
    RawIrcMsg(..)
  , TagEntry(..)
  , rawIrcMsg
  , msgTags
  , msgPrefix
  , msgCommand
  , msgParams

  -- * Text format for IRC messages
  , parseRawIrcMsg
  , renderRawIrcMsg
  , prefixParser
  , simpleTokenParser

  -- * Permissive text decoder
  , asUtf8
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Attoparsec.Text as P
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

import           Irc.UserInfo

-- | 'RawIrcMsg' breaks down the IRC protocol into its most basic parts.
-- The "trailing" parameter indicated in the IRC protocol with a leading
-- colon will appear as the last parameter in the parameter list.
--
-- Note that RFC 2812 specifies a maximum of 15 parameters.
--
-- This parser is permissive regarding spaces. It aims to parse carefully
-- constructed messages exactly and to make a best effort to recover from
-- extraneous spaces. It makes no effort to validate nicknames, usernames,
-- hostnames, commands, etc. Servers don't all agree on these things.
--
-- @:prefix COMMAND param0 param1 param2 .. paramN@
data RawIrcMsg = RawIrcMsg
  { _msgTags       :: [TagEntry] -- ^ IRCv3.2 message tags
  , _msgPrefix     :: Maybe UserInfo -- ^ Optional sender of message
  , _msgCommand    :: !Text -- ^ command
  , _msgParams     :: [Text] -- ^ command parameters
  }
  deriving (Read, Show)

-- | Key value pair representing an IRCv3.2 message tag.
-- The value in this pair has had the message tag unescape
-- algorithm applied.
data TagEntry = TagEntry {-# UNPACK #-} !Text {-# UNPACK #-} !Text
  deriving (Read, Show)

makeLenses ''RawIrcMsg

-- | Attempt to split an IRC protocol message without its trailing newline
-- information into a structured message.
parseRawIrcMsg :: Text -> Maybe RawIrcMsg
parseRawIrcMsg x =
  case parseOnly rawIrcMsgParser x of
    Left{}  -> Nothing
    Right r -> Just r

-- | RFC 2812 specifies that there can only be up to
-- 14 "middle" parameters, after that the fifteenth is
-- the final parameter and the trailing : is optional!
maxMiddleParams :: Int
maxMiddleParams = 14

--  Excerpt from https://tools.ietf.org/html/rfc2812#section-2.3.1

--  message    =  [ ":" prefix SPACE ] command [ params ] crlf
--  prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )
--  command    =  1*letter / 3digit
--  params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
--             =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]

--  nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
--                  ; any octet except NUL, CR, LF, " " and ":"
--  middle     =  nospcrlfcl *( ":" / nospcrlfcl )
--  trailing   =  *( ":" / " " / nospcrlfcl )

--  SPACE      =  %x20        ; space character
--  crlf       =  %x0D %x0A   ; "carriage return" "linefeed"

-- | Parse a whole IRC message assuming that the trailing
-- newlines have already been removed. This parser will
-- parse valid messages correctly but will also accept some
-- invalid messages. Presumably the server isn't sending
-- invalid messages!
rawIrcMsgParser :: Parser RawIrcMsg
rawIrcMsgParser =
  do tags   <- fromMaybe [] <$> guarded '@' tagsParser
     prefix <- guarded ':' prefixParser
     cmd    <- simpleTokenParser
     params <- paramsParser maxMiddleParams
     return $! RawIrcMsg
       { _msgTags    = tags
       , _msgPrefix  = prefix
       , _msgCommand = cmd
       , _msgParams  = params
       }

-- | Parse the list of parameters in a raw message. The RFC
-- allows for up to 15 parameters.
paramsParser ::
  Int {- ^ possible middle parameters -} -> Parser [Text]
paramsParser !n =
  do end <- P.atEnd
     if end
       then return []
       else do isColon <- optionalChar ':'
               if isColon || n == 0
                 then finalParam
                 else middleParam

  where

  finalParam =
    do x <- takeText
       let !x' = Text.copy x
       return [x']

  middleParam =
    do x  <- simpleTokenParser
       xs <- paramsParser (n-1)
       return (x:xs)

tagsParser :: Parser [TagEntry]
tagsParser = tagParser `sepBy1` (char ';') <* char ' '

tagParser :: Parser TagEntry
tagParser =
  do key <- P.takeWhile (notInClass " =;")
     hasValue <- optionalChar '='
     val <- if hasValue
              then unescapeTagVal <$> P.takeWhile (notInClass " ;")
              else return ""
     return $! TagEntry key val


unescapeTagVal :: Text -> Text
unescapeTagVal = Text.pack . aux . Text.unpack
  where
    aux ('\\':':':xs) = ';':aux xs
    aux ('\\':'s':xs) = ' ':aux xs
    aux ('\\':'\\':xs) = '\\':aux xs
    aux ('\\':'r':xs) = '\r':aux xs
    aux ('\\':'n':xs) = '\n':aux xs
    aux (x:xs)        = x : aux xs
    aux ""            = ""

escapeTagVal :: Text -> Text
escapeTagVal = Text.concatMap aux
  where
    aux ';'  = "\\:"
    aux ' '  = "\\s"
    aux '\\' = "\\\\"
    aux '\r' = "\\r"
    aux '\n' = "\\n"
    aux x = Text.singleton x

-- | Parse a rendered 'UserInfo' token.
prefixParser :: Parser UserInfo
prefixParser =
  do tok <- simpleTokenParser
     return $! parseUserInfo tok

-- | Take the next space-delimited lexeme
simpleTokenParser :: Parser Text
simpleTokenParser =
  do xs <- P.takeWhile1 (/= ' ')
     P.skipWhile (== ' ')
     return $! Text.copy xs


-- | Serialize a structured IRC protocol message back into its wire
-- format. This command adds the required trailing newline.
renderRawIrcMsg :: RawIrcMsg -> ByteString
renderRawIrcMsg !m
   = L.toStrict
   $ Builder.toLazyByteString
   $ renderTags (view msgTags m)
  <> maybe mempty renderPrefix (view msgPrefix m)
  <> Text.encodeUtf8Builder (view msgCommand m)
  <> buildParams (view msgParams m)
  <> Builder.char8 '\r'
  <> Builder.char8 '\n'

-- | Construct a new 'RawIrcMsg' without a time or prefix.
rawIrcMsg ::
  Text {- ^ command -} ->
  [Text] {- ^ parameters -} -> RawIrcMsg
rawIrcMsg = RawIrcMsg [] Nothing

renderTags :: [TagEntry] -> Builder
renderTags [] = mempty
renderTags xs
    = Builder.char8 '@'
   <> mconcat (intersperse (Builder.char8 ';') (map renderTag xs))
   <> Builder.char8 ' '

renderTag :: TagEntry -> Builder
renderTag (TagEntry key val)
   = Text.encodeUtf8Builder key
  <> Builder.char8 '='
  <> Text.encodeUtf8Builder (escapeTagVal val)

renderPrefix :: UserInfo -> Builder
renderPrefix u
   = Builder.char8 ':'
  <> Text.encodeUtf8Builder (renderUserInfo u)
  <> Builder.char8 ' '

-- | Build concatenate a list of parameters into a single, space-
-- delimited bytestring. Use a colon for the last parameter if it contains
-- a colon or a space.
buildParams :: [Text] -> Builder
buildParams [x]
  | " " `Text.isInfixOf` x || ":" `Text.isPrefixOf` x
  = Builder.char8 ' ' <> Builder.char8 ':' <> Text.encodeUtf8Builder x
buildParams (x:xs)
  = Builder.char8 ' ' <> Text.encodeUtf8Builder x <> buildParams xs
buildParams [] = mempty

-- | When the current input matches the given character parse
-- using the given parser.
guarded :: Char -> Parser b -> Parser (Maybe b)
guarded c p =
  do success <- optionalChar c
     if success then Just <$> p else pure Nothing


-- | Returns 'True' iff next character in stream matches argument.
optionalChar :: Char -> Parser Bool
optionalChar c = True <$ char c <|> pure False


-- | Try to decode a message as UTF-8. If that fails interpret it as Windows CP1252
-- This helps deal with clients like XChat that get clever and otherwise misconfigured
-- clients.
asUtf8 :: ByteString -> Text
asUtf8 x = case Text.decodeUtf8' x of
             Right txt -> txt
             Left{}    -> decodeCP1252 x

decodeCP1252 :: ByteString -> Text
decodeCP1252 bs = Text.pack [ cp1252 Vector.! fromIntegral x | x <- B.unpack bs ]

-- This character encoding is a superset of ISO 8859-1 in terms of printable
-- characters, but differs from the IANA's ISO-8859-1 by using displayable
-- characters rather than control characters in the 80 to 9F (hex) range.
cp1252 :: Vector Char
cp1252 = Vector.fromList
       $ ['\x00'..'\x7f']
      ++ "€\x81‚ƒ„…†‡ˆ‰Š‹Œ\x8dŽ\x8f\x90‘’“”•–—˜™š›œ\x9džŸ"
      ++ ['\xa0'..'\xff']
