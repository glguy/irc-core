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
  , rawIrcMsg
  , msgServerTime
  , msgPrefix
  , msgCommand
  , msgParams

  -- * Text format for IRC messages
  , parseRawIrcMsg
  , renderRawIrcMsg

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
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
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
  { _msgServerTime :: Maybe UTCTime -- ^ Time from znc.in/server-time-iso extension
  , _msgPrefix     :: Maybe UserInfo -- ^ Optional sender of message
  , _msgCommand    :: !Text -- ^ command
  , _msgParams     :: [Text] -- ^ command parameters
  }
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
  do time   <- guarded (string "@time=") timeParser
     prefix <- guarded (char ':') prefixParser
     cmd    <- simpleTokenParser
     params <- paramsParser maxMiddleParams
     return $! RawIrcMsg
       { _msgServerTime = time
       , _msgPrefix     = prefix
       , _msgCommand    = cmd
       , _msgParams     = params
       }

-- | Parse the list of parameters in a raw message. The RFC
-- allows for up to 15 parameters.
paramsParser ::
  Int {- ^ possible middle parameters -} -> Parser [Text]
paramsParser !n =
  do end <- P.atEnd
     if end
       then return []
       else do mbColon <- optional (char ':')
               if n == 0 || isJust mbColon
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

-- | Parse the server-time message prefix:
-- @time=2015-03-04T22:29:04.064Z
timeParser :: Parser UTCTime
timeParser =
  do timeBytes <- simpleTokenParser
     case parseIrcTime (Text.unpack timeBytes) of
       Nothing -> fail "Bad server-time format"
       Just t  -> return t

parseIrcTime :: String -> Maybe UTCTime
parseIrcTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z"

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

-- | Take the bytes up to the next space delimiter.
-- If the first character of this token is a ':'
-- then take the whole remaining bytestring



-- | Serialize a structured IRC protocol message back into its wire
-- format. This command adds the required trailing newline.
renderRawIrcMsg :: RawIrcMsg -> ByteString
renderRawIrcMsg m = L.toStrict $ Builder.toLazyByteString $
     maybe mempty renderPrefix (view msgPrefix m)
  <> Text.encodeUtf8Builder (view msgCommand m)
  <> buildParams (view msgParams m)
  <> Builder.char8 '\r'
  <> Builder.char8 '\n'

-- | Construct a new 'RawIrcMsg' without a time or prefix.
rawIrcMsg ::
  Text {- ^ command -} ->
  [Text] {- ^ parameters -} -> RawIrcMsg
rawIrcMsg = RawIrcMsg Nothing Nothing

renderPrefix :: UserInfo -> Builder
renderPrefix u = Builder.char8 ':'
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

-- | When the first parser succeeds require the second parser to succeed.
-- Otherwise return Nothing
guarded :: Parser a -> Parser b -> Parser (Maybe b)
guarded pa pb =
  do mb <- optional pa
     case mb of
       Nothing -> return Nothing
       Just{}  -> fmap Just pb


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
cp1252 = Vector.fromListN 256
  ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK','\a','\b','\t','\n','\v','\f','\r','\SO','\SI',
   '\DLE','\DC1','\DC2','\DC3','\DC4','\NAK','\SYN','\ETB','\CAN','\EM','\SUB','\ESC','\FS','\GS','\RS','\US',
   ' ','!','\"','#','$','%','&','\'','(',')','*','+',',','-','.','/',
   '0','1','2','3','4','5','6','7','8','9',':',';','<','=','>','?',
   '@','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O',
   'P','Q','R','S','T','U','V','W','X','Y','Z','[','\\',']','^','_',
   '`','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o',
   'p','q','r','s','t','u','v','w','x','y','z','{','|','}','~','\DEL',
   '\8364','\129','\8218','\402','\8222','\8230','\8224','\8225','\710','\8240','\352','\8249','\338','\141','\381','\143',
   '\144','\8216','\8217','\8220','\8221','\8226','\8211','\8212','\732','\8482','\353','\8250','\339','\157','\382','\376',
   '\160','\161','\162','\163','\164','\165','\166','\167','\168','\169','\170','\171','\172','\173','\174','\175',
   '\176','\177','\178','\179','\180','\181','\182','\183','\184','\185','\186','\187','\188','\189','\190','\191',
   '\192','\193','\194','\195','\196','\197','\198','\199','\200','\201','\202','\203','\204','\205','\206','\207',
   '\208','\209','\210','\211','\212','\213','\214','\215','\216','\217','\218','\219','\220','\221','\222','\223',
   '\224','\225','\226','\227','\228','\229','\230','\231','\232','\233','\234','\235','\236','\237','\238','\239',
   '\240','\241','\242','\243','\244','\245','\246','\247','\248','\249','\250','\251','\252','\253','\254','\255']
