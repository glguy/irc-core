{-# Language OverloadedStrings, BangPatterns, ViewPatterns #-}
{-|
Module      : Client.Image.Message
Description : Renderer for message lines
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides image renderers for messages.

-}
module Client.Image.Message
  ( MessageRendererParams(..)
  , RenderMode(..)
  , IdentifierColorMode(..)
  , defaultRenderParams
  , msgImage
  , metadataImg
  , ignoreImage
  , quietIdentifier
  , coloredUserInfo
  , coloredIdentifier
  , cleanText
  , cleanChar
  , nickPad
  , timeImage
  , drawWindowLine
  , parseIrcTextWithNicks
  ) where

import           Client.Configuration (PaddingMode(..))
import           Client.Image.LineWrap
import           Client.Image.MircFormatting
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.Message
import           Client.State.DCC (isSend)
import           Client.State.Window
import           Client.UserHost
import           Control.Lens
import           Data.Char
import           Data.Hashable (hash)
import           Data.HashMap.Strict (HashMap)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time
import qualified Data.Vector as Vector
import           Graphics.Vty.Attributes
import           Irc.Codes
import           Irc.Identifier
import           Irc.Message
import           Irc.RawIrcMsg
import           Irc.UserInfo
import           Text.Read

-- | Parameters used when rendering messages
data MessageRendererParams = MessageRendererParams
  { rendStatusMsg  :: [Char] -- ^ restricted message sigils
  , rendUserSigils :: [Char] -- ^ sender sigils
  , rendNicks      :: HashSet Identifier -- ^ nicknames to highlight
  , rendMyNicks    :: HashSet Identifier -- ^ nicknames to highlight in red
  , rendPalette    :: Palette -- ^ nick color palette
  , rendAccounts   :: Maybe (HashMap Identifier UserAndHost)
  }

-- | Default 'MessageRendererParams' with no sigils or nicknames specified
defaultRenderParams :: MessageRendererParams
defaultRenderParams = MessageRendererParams
  { rendStatusMsg   = ""
  , rendUserSigils  = ""
  , rendNicks       = HashSet.empty
  , rendMyNicks     = HashSet.empty
  , rendPalette     = defaultPalette
  , rendAccounts    = Nothing
  }


-- | Construct a message given the time the message was received and its
-- render parameters.
msgImage ::
  ZonedTime                {- ^ time of message     -} ->
  MessageRendererParams    {- ^ render parameters   -} ->
  MessageBody              {- ^ message body        -} ->
  (Image', Image', Image') {- ^ prefix, image, full -}
msgImage when params body = (prefix, image, full)
  where
    si = statusMsgImage (rendStatusMsg params)

    prefix = si <> prefixImage params body

    image = bodyImage NormalRender params body

    full =
      mconcat
       [ datetimeImage (rendPalette params) when
       , statusMsgImage (rendStatusMsg params)
       , bodyImage DetailedRender params body
       ]

cleanChar :: Char -> Char
cleanChar x
  | x < '\x20'  = chr (0x2400 + ord x) -- ␀ .. ␙
  | x == '\DEL' = '\x2421' -- ␡
  | isControl x = '\xfffd' -- �
  | otherwise   = x

cleanText :: Text -> Text
cleanText = Text.map cleanChar

errorPrefix ::
  MessageRendererParams ->
  Image'
errorPrefix params =
  text' (view palError (rendPalette params)) "error" <>
  char defAttr ':'


normalPrefix :: MessageRendererParams -> Image'
normalPrefix params =
  text' (view palLabel (rendPalette params)) "client" <>
  char defAttr ':'


-- | Render the sigils for a restricted message.
statusMsgImage :: [Char] {- ^ sigils -} -> Image'
statusMsgImage modes
  | null modes = mempty
  | otherwise  = "(" <> string statusMsgColor modes <> ") "
  where
    statusMsgColor = withForeColor defAttr red


-- | Render a 'MessageBody' given the sender's sigils and the nicknames to
-- highlight.
prefixImage ::
  MessageRendererParams ->
  MessageBody -> Image'
prefixImage params body =
  case body of
    IrcBody irc  -> ircLinePrefix params irc
    ErrorBody{}  -> errorPrefix  params
    NormalBody{} -> normalPrefix params

-- | Render a 'MessageBody' given the sender's sigils and the nicknames to
-- highlight.
bodyImage ::
  RenderMode ->
  MessageRendererParams ->
  MessageBody -> Image'
bodyImage rm params body =
  case body of
    IrcBody irc | NormalRender   <- rm -> ircLineImage     params irc
                | DetailedRender <- rm -> fullIrcLineImage params irc
    ErrorBody  txt                     -> parseIrcText txt
    NormalBody txt                     -> parseIrcText txt

-- | Render a 'ZonedTime' as time using quiet attributes
--
-- @
-- 23:15
-- @
timeImage :: Palette -> TimeOfDay -> Image'
timeImage palette
  = string (view palTime palette)
  . formatTime defaultTimeLocale "%R "

-- | Render a 'ZonedTime' as full date and time user quiet attributes.
-- Excludes the year.
--
-- @
-- 07-24 23:15:10
-- @
datetimeImage :: Palette -> ZonedTime -> Image'
datetimeImage palette
  = string (view palTime palette)
  . formatTime defaultTimeLocale "%m-%d %T "

-- | Level of detail to use when rendering
data RenderMode
  = NormalRender -- ^ only render nicknames
  | DetailedRender -- ^ render full user info

-- | Optionally add padding to an input image according to the
-- specified mode. If the input image is already wider than
-- the specified padding mode, the image is returned unmodified.
nickPad ::
  PaddingMode {- ^ padding mode -} ->
  Image'      {- ^ input image  -} ->
  Image'      {- ^ padded image -}
nickPad mode img =
  case mode of
    LeftPadding  w | w > iw -> mkpad (w-iw) <> img
    RightPadding w | w > iw -> img <> mkpad (w-iw)
    _                       -> img
  where
    iw = imageWidth img
    mkpad n = string defAttr (replicate n ' ')


-- | Render the sender of a message in normal mode.
-- This is typically something like @\@nickname:@
ircLinePrefix ::
  MessageRendererParams ->
  IrcMsg -> Image'
ircLinePrefix !rp body =
  let pal     = rendPalette rp
      sigils  = rendUserSigils rp
      myNicks = rendMyNicks rp
      rm      = NormalRender

      who n   = string (view palSigil pal) sigils <> ui
        where
          baseUI    = coloredUserInfo pal rm myNicks n
          ui = case rendAccounts rp of
                 Nothing -> baseUI -- not tracking any accounts
                 Just accts ->
                   let isKnown acct = not (Text.null acct || acct == "*")
                       mbAcct = accts
                             ^? ix (userNick n)
                              . uhAccount
                              . filtered isKnown in
                   case mbAcct of
                     Just acct
                       | mkId acct == userNick n -> baseUI
                       | otherwise -> baseUI <> "(" <> text' defAttr (cleanText acct) <> ")"
                     Nothing -> "~" <> baseUI
  in
  case body of
    Join       {} -> mempty
    Part       {} -> mempty
    Quit       {} -> mempty
    Ping       {} -> mempty
    Pong       {} -> mempty
    Nick       {} -> mempty

    Topic src _ _ ->
      who src <> " changed the topic:"

    Kick kicker _channel kickee _reason ->
      who kicker <>
      " kicked " <>
      coloredIdentifier pal NormalIdentifier myNicks kickee <>
      ":"

    Notice src _ _ ->
      who src <>
      string (withForeColor defAttr red) ":"

    Privmsg src _ _ -> who src <> ":"
    Wallops src _   ->
      string (withForeColor defAttr red) "WALL " <> who src <> ":"

    Ctcp src _dst "ACTION" _txt ->
      string (withForeColor defAttr blue) "* " <> who src
    Ctcp src _dst "DCC" txt | isSend txt ->
      who src <> " offers a DCC transfer"
    Ctcp {} -> mempty

    CtcpNotice src _dst _cmd _txt ->
      string (withForeColor defAttr red) "! " <> who src

    Error {} -> string (view palError pal) "ERROR" <> ":"

    Reply code _ -> replyCodePrefix code

    UnknownMsg irc ->
      case view msgPrefix irc of
        Just ui -> who ui
        Nothing -> string (view palError pal) "?"

    Cap cmd ->
      text' (withForeColor defAttr magenta) (renderCapCmd cmd) <> ":"

    Mode nick _ _ -> who nick <> " set mode:"

    Authenticate{} -> "AUTHENTICATE"
    BatchStart{}   -> mempty
    BatchEnd{}     -> mempty

    Account user _ -> who user <> " account:"
    Chghost ui _ _ -> who ui <> " chghost:"


-- | Render a chat message given a rendering mode, the sigils of the user
-- who sent the message, and a list of nicknames to highlight.
ircLineImage ::
  MessageRendererParams ->
  IrcMsg -> Image'
ircLineImage !rp body =
  let pal     = rendPalette rp
      myNicks = rendMyNicks rp
      nicks   = rendNicks rp
  in
  case body of
    Join        {} -> mempty
    Part        {} -> mempty
    Quit        {} -> mempty
    Ping        {} -> mempty
    Pong        {} -> mempty
    BatchStart  {} -> mempty
    BatchEnd    {} -> mempty
    Nick        {} -> mempty
    Authenticate{} -> "***"

    Error                   txt -> parseIrcText txt
    Topic      _ _          txt -> parseIrcTextWithNicks pal myNicks nicks False txt
    Kick       _ _ _        txt -> parseIrcTextWithNicks pal myNicks nicks False txt
    Notice     _ _          txt -> parseIrcTextWithNicks pal myNicks nicks False txt
    Privmsg    _ _          txt -> parseIrcTextWithNicks pal myNicks nicks False txt
    Wallops    _            txt -> parseIrcTextWithNicks pal myNicks nicks False txt
    Ctcp       _ _ "ACTION" txt -> parseIrcTextWithNicks pal myNicks nicks False txt
    Ctcp {}                     -> mempty
    CtcpNotice _ _ cmd      txt -> parseIrcText cmd <> " " <>
                                   parseIrcTextWithNicks pal myNicks nicks False txt

    Reply code params -> renderReplyCode pal NormalRender code params
    UnknownMsg irc ->
      text' defAttr (view msgCommand irc) <>
      char defAttr ' ' <>
      separatedParams (view msgParams irc)
    Cap cmd           -> text' defAttr (cleanText (capCmdText cmd))
    Mode _ _ params   -> ircWords params

    Account _ acct -> if Text.null acct then "*" else text' defAttr (cleanText acct)
    Chghost _ user host -> text' defAttr (cleanText user) <> " " <> text' defAttr (cleanText host)

-- | Render a chat message given a rendering mode, the sigils of the user
-- who sent the message, and a list of nicknames to highlight.
fullIrcLineImage ::
  MessageRendererParams ->
  IrcMsg -> Image'
fullIrcLineImage !rp body =
  let quietAttr = view palMeta pal
      pal     = rendPalette rp
      sigils  = rendUserSigils rp
      myNicks = rendMyNicks rp
      nicks   = rendNicks rp
      rm      = DetailedRender

      plainWho = coloredUserInfo pal rm myNicks

      who n =
        -- sigils
        string (view palSigil pal) sigils <>

        -- nick!user@host
        plainWho n <>

        case rendAccounts rp ^? folded . ix (userNick n) . uhAccount of
          Just acct
            | not (Text.null acct) -> text' quietAttr ("(" <> cleanText acct <> ")")
          _ -> ""
  in
  case body of
    Nick old new ->
      string quietAttr "nick " <>
      who old <>
      " is now known as " <>
      coloredIdentifier pal NormalIdentifier myNicks new

    Join nick _chan acct gecos ->
      string quietAttr "join " <>
      plainWho nick <>
      accountPart <> gecosPart
      where
        accountPart
          | Text.null acct = mempty
          | otherwise      = text' quietAttr ("(" <> cleanText acct <> ")")
        gecosPart
          | Text.null gecos = mempty
          | otherwise       = text' quietAttr (" [" <> cleanText gecos <> "]")

    Part nick _chan mbreason ->
      string quietAttr "part " <>
      who nick <>
      foldMap (\reason -> string quietAttr " (" <>
                          parseIrcText reason <>
                          string quietAttr ")") mbreason

    Quit nick mbreason ->
      string quietAttr "quit "   <>
      who nick <>
      foldMap (\reason -> string quietAttr " (" <>
                          parseIrcText reason <>
                          string quietAttr ")") mbreason

    Kick kicker _channel kickee reason ->
      string quietAttr "kick " <>
      who kicker <>
      " kicked " <>
      coloredIdentifier pal NormalIdentifier myNicks kickee <>
      ": " <>
      parseIrcText reason

    Topic src _dst txt ->
      string quietAttr "tpic " <>
      who src <>
      " changed the topic: " <>
      parseIrcText txt

    Notice src _dst txt ->
      string quietAttr "note " <>
      who src <>
      string (withForeColor defAttr red) ": " <>
      parseIrcTextWithNicks pal myNicks nicks False txt

    Privmsg src _dst txt ->
      string quietAttr "chat " <>
      who src <> ": " <>
      parseIrcTextWithNicks pal myNicks nicks False txt

    Wallops src txt ->
      string quietAttr "wall " <>
      who src <> ": " <>
      parseIrcTextWithNicks pal myNicks nicks False txt

    Ctcp src _dst "ACTION" txt ->
      string quietAttr "actp " <>
      string (withForeColor defAttr blue) "* " <>
      who src <> " " <>
      parseIrcTextWithNicks pal myNicks nicks False txt

    Ctcp src _dst cmd txt ->
      string quietAttr "ctcp " <>
      string (withForeColor defAttr blue) "! " <>
      who src <> " " <>
      parseIrcText cmd <>
      if Text.null txt then mempty else separatorImage <> parseIrcText txt

    CtcpNotice src _dst cmd txt ->
      string quietAttr "ctcp " <>
      string (withForeColor defAttr red) "! " <>
      who src <> " " <>
      parseIrcText cmd <>
      if Text.null txt then mempty else separatorImage <> parseIrcText txt

    Ping params ->
      "PING " <> separatedParams params

    Pong params ->
      "PONG " <> separatedParams params

    Error reason ->
      string (view palError pal) "ERROR " <>
      parseIrcText reason

    Reply code params ->
      renderReplyCode pal DetailedRender code params

    UnknownMsg irc ->
      foldMap (\ui -> coloredUserInfo pal rm myNicks ui <> char defAttr ' ')
        (view msgPrefix irc) <>
      text' defAttr (view msgCommand irc) <>
      char defAttr ' ' <>
      separatedParams (view msgParams irc)

    Cap cmd ->
      text' (withForeColor defAttr magenta) (renderCapCmd cmd) <>
      text' defAttr ": " <>
      text' defAttr (cleanText (capCmdText cmd))

    Mode nick _chan params ->
      string quietAttr "mode " <>
      who nick <> " set mode: " <>
      ircWords params

    Authenticate{} -> "AUTHENTICATE ***"
    BatchStart{}   -> "BATCH +"
    BatchEnd{}     -> "BATCH -"

    Account src acct ->
      string quietAttr "acct " <>
      who src <> ": " <>
      if Text.null acct then "*" else text' defAttr (cleanText acct)

    Chghost user newuser newhost ->
      string quietAttr "chng " <>
      who user <> ": " <>
      text' defAttr (cleanText newuser) <> " " <> text' defAttr (cleanText newhost)


renderCapCmd :: CapCmd -> Text
renderCapCmd cmd =
  case cmd of
    CapLs   {} -> "caps-available"
    CapList {} -> "caps-active"
    CapAck  {} -> "caps-acknowledged"
    CapNak  {} -> "caps-rejected"
    CapNew  {} -> "caps-offered"
    CapDel  {} -> "caps-withdrawn"

separatorImage :: Image'
separatorImage = char (withForeColor defAttr blue) '·'

-- | Process list of 'Text' as individual IRC formatted words
-- separated by a special separator to distinguish parameters
-- from words within parameters.
separatedParams :: [Text] -> Image'
separatedParams = mconcat . intersperse separatorImage . map parseIrcText

-- | Process list of 'Text' as individual IRC formatted words
ircWords :: [Text] -> Image'
ircWords = mconcat . intersperse (char defAttr ' ') . map parseIrcText

replyCodePrefix :: ReplyCode -> Image'
replyCodePrefix code =
  text' attr (replyCodeText info) <>
  char defAttr ':'
  where
    info = replyCodeInfo code

    color = case replyCodeType info of
              ClientServerReply -> magenta
              CommandReply      -> green
              ErrorReply        -> red
              UnknownReply      -> yellow

    attr = withForeColor defAttr color

renderReplyCode :: Palette -> RenderMode -> ReplyCode -> [Text] -> Image'
renderReplyCode pal rm code@(ReplyCode w) params =
  case rm of
    DetailedRender -> string attr (shows w " ") <> rawParamsImage
    NormalRender   ->
      case code of
        RPL_WHOISUSER    -> whoisUserParamsImage
        RPL_WHOWASUSER   -> whoisUserParamsImage
        RPL_WHOISACTUALLY-> param_3_4_Image
        RPL_WHOISIDLE    -> whoisIdleParamsImage
        RPL_WHOISCHANNELS-> param_3_3_Image
        RPL_WHOISACCOUNT -> param_3_4_Image
        RPL_WHOISSERVER  -> whoisServerParamsImage
        RPL_WHOISSECURE  -> param_3_3_Image
        RPL_WHOISOPERATOR-> param_3_3_Image
        RPL_WHOISCERTFP  -> param_3_3_Image
        RPL_WHOISSPECIAL -> param_3_3_Image
        RPL_WHOISHOST    -> param_3_3_Image
        RPL_ENDOFWHOIS   -> ""
        RPL_ENDOFWHOWAS  -> ""
        RPL_TOPIC        -> param_3_3_Image
        RPL_TOPICWHOTIME -> topicWhoTimeParamsImage
        RPL_CHANNEL_URL  -> param_3_3_Image
        RPL_CREATIONTIME -> creationTimeParamsImage
        RPL_INVITING     -> params_2_3_Image
        RPL_TESTLINE     -> testlineParamsImage
        RPL_STATSLINKINFO-> linkInfoParamsImage
        RPL_STATSILINE   -> authLineParamsImage
        _                -> rawParamsImage
  where
    label t = text' (view palLabel pal) t <> ": "

    rawParamsImage = separatedParams params'

    params' = case rm of
                DetailedRender -> params
                NormalRender   -> drop 1 params

    info = replyCodeInfo code

    color = case replyCodeType info of
              ClientServerReply -> magenta
              CommandReply      -> green
              ErrorReply        -> red
              UnknownReply      -> yellow

    attr = withForeColor defAttr color

    params_2_3_Image =
      case params of
        [_, p, _] -> parseIrcText' False p
        _         -> rawParamsImage

    param_3_3_Image =
      case params of
        [_, _, txt] -> parseIrcText' False txt
        _           -> rawParamsImage

    param_3_4_Image =
      case params of
        [_, _, p, _] -> parseIrcText' False p
        _            -> rawParamsImage

    topicWhoTimeParamsImage =
      case params of
        [_, _, who, time] ->
          label "set by" <>
          text' defAttr who <>
          label " at" <>
          string defAttr (prettyUnixTime (Text.unpack time))
        _ -> rawParamsImage

    creationTimeParamsImage =
      case params of
        [_, _, time, _] -> string defAttr (prettyUnixTime (Text.unpack time))
        _ -> rawParamsImage

    whoisUserParamsImage =
      case params of
        [_, nick, user, host, _, real] ->
          text' (withStyle defAttr bold) nick <>
          text' (view palLabel pal) "!" <>
          parseIrcText' False user <>
          text' (view palLabel pal) "@" <>
          parseIrcText' False host <>
          label " gecos" <>
          parseIrcText' False real
        _ -> rawParamsImage

    whoisIdleParamsImage =
      case params of
        [_, _, idle, signon, _txt] ->
          string defAttr (prettyTime 1 (Text.unpack idle)) <>
          label " sign-on" <>
          string defAttr (prettyUnixTime (Text.unpack signon))
        _ -> rawParamsImage

    whoisServerParamsImage =
      case params of
        [_, _, host, txt] ->
          parseIrcText' False host <>
          label " note" <>
          parseIrcText' False txt
        _ -> rawParamsImage

    testlineParamsImage =
      case params of
        [_, name, mins, mask, msg] ->
          text' defAttr name <>
          label " duration" <>
          string defAttr (prettyTime 60 (Text.unpack mins)) <>
          label " mask" <>
          text' defAttr mask <>
          label " reason" <>
          text' defAttr msg
        _ -> rawParamsImage

    linkInfoParamsImage =
      case params of
        [_, name, sendQ, sendM, sendK, recvM, recvK, Text.words -> conn : idle : caps] ->
          text' defAttr name <>
          label " sendQ" <> text' defAttr sendQ <>
          label " sendM" <> text' defAttr sendM <>
          label " sendK" <> text' defAttr sendK <>
          label " recvM" <> text' defAttr recvM <>
          label " recvK" <> text' defAttr recvK <>
          label " since" <> string defAttr (prettyTime 1 (Text.unpack conn)) <>
          label " idle"  <> string defAttr (prettyTime 1 (Text.unpack idle)) <>
          label " caps"  <> text' defAttr (Text.unwords caps)
        _ -> rawParamsImage

    authLineParamsImage =
      case params of
        [_, "I", name, pass, mask, port, klass] ->
          text' defAttr name <>
          (if pass == "<NULL>" then mempty else label " pass" <> text' defAttr pass) <>
          label " mask" <> text' defAttr mask' <>
          (if port == "0" then mempty else label " port" <> text' defAttr port) <>
          label " class" <> text' defAttr klass <>
          (if null special then mempty else
            label " special" <> text' defAttr (Text.unwords special))
          where
            (mask', special) = parseILinePrefix mask
        _ -> rawParamsImage

parseILinePrefix :: Text -> (Text, [Text])
parseILinePrefix = go []
  where
    go special mask =
      case Text.uncons mask of
        Just (getSpecial -> Just s, mask') -> go (s:special) mask'
        _ -> (mask, reverse special)

    getSpecial x =
      case x of
        '-' -> Just "no-tilde"
        '+' -> Just "need-ident"
        '=' -> Just "spoof-IP"
        '|' -> Just "flood-exempt"
        '$' -> Just "dnsbl-exempt"
        '^' -> Just "kline-exempt"
        '>' -> Just "limits-exempt"
        _   -> Nothing


-- | Transform string representing seconds in POSIX time to pretty format.
prettyUnixTime :: String -> String
prettyUnixTime str =
  case parseTimeM False defaultTimeLocale "%s" str of
    Nothing -> str
    Just t  -> formatTime defaultTimeLocale "%A %B %m, %Y %H:%M:%S %Z" (t :: UTCTime)

-- | Render string representing seconds into days, hours, minutes, and seconds.
prettyTime :: Int -> String -> String
prettyTime scale str =
  case readMaybe str of
    Nothing -> str
    Just 0  -> "0s"
    Just n  -> intercalate " "
             $ map (\(u,i) -> show i ++ [u])
             $ filter (\x -> snd x /= 0)
             $ zip "dhms" [d,h,m,s :: Int]
      where
        n0     = n * scale
        (n1,s) = quotRem n0 60
        (n2,m) = quotRem n1 60
        (d ,h) = quotRem n2 24


data IdentifierColorMode
  = PrivmsgIdentifier -- ^ An identifier in a PRIVMSG
  | NormalIdentifier  -- ^ An identifier somewhere else

-- | Render a nickname in its hash-based color.
coloredIdentifier ::
  Palette             {- ^ color palette      -} ->
  IdentifierColorMode {- ^ draw mode          -} ->
  HashSet Identifier  {- ^ my nicknames       -} ->
  Identifier          {- ^ identifier to draw -} ->
  Image'
coloredIdentifier palette icm myNicks ident =
  text' color (idText ident)
  where
    color
      | ident `HashSet.member` myNicks =
          case icm of
            PrivmsgIdentifier -> view palSelfHighlight palette
            NormalIdentifier  -> view palSelf palette

      | otherwise = v Vector.! i

    v = view palNicks palette
    i = hash ident `mod` Vector.length v

-- | Render an a full user. In normal mode only the nickname will be rendered.
-- If detailed mode the full user info including the username and hostname parts
-- will be rendered. The nickname will be colored.
coloredUserInfo ::
  Palette            {- ^ color palette   -} ->
  RenderMode         {- ^ mode            -} ->
  HashSet Identifier {- ^ my nicks        -} ->
  UserInfo           {- ^ userinfo to draw-} ->
  Image'
coloredUserInfo palette NormalRender myNicks ui =
  coloredIdentifier palette NormalIdentifier myNicks (userNick ui)
coloredUserInfo palette DetailedRender myNicks !ui =
  mconcat
    [ coloredIdentifier palette NormalIdentifier myNicks (userNick ui)
    , aux '!' (userName ui)
    , aux '@' (userHost ui)
    ]
  where
    quietAttr = view palMeta palette
    aux x xs
      | Text.null xs = mempty
      | otherwise    = char quietAttr x <> text' quietAttr xs

-- | Render an identifier without using colors. This is useful for metadata.
quietIdentifier :: Palette -> Identifier -> Image'
quietIdentifier palette ident =
  text' (view palMeta palette) (idText ident)

-- | Parse message text to construct an image. If the text has formatting
-- control characters in it then the text will be rendered according to
-- the formatting codes. Otherwise the nicknames in the message are
-- highlighted.
parseIrcTextWithNicks ::
  Palette            {- ^ palette      -} ->
  HashSet Identifier {- ^ my nicks     -} ->
  HashSet Identifier {- ^ other nicks  -} ->
  Bool               {- ^ explicit controls rendering -} ->
  Text               {- ^ input text   -} ->
  Image'             {- ^ colored text -}
parseIrcTextWithNicks palette myNick nicks explicit txt
  | Text.any isControl txt = parseIrcText' explicit txt
  | otherwise              = highlightNicks palette myNick nicks txt

-- | Given a list of nicknames and a chat message, this will generate
-- an image where all of the occurrences of those nicknames are colored.
highlightNicks ::
  Palette ->
  HashSet Identifier {- ^ my nicks    -} ->
  HashSet Identifier {- ^ other nicks -} ->
  Text -> Image'
highlightNicks palette myNicks nicks txt = foldMap highlight1 txtParts
  where
    txtParts = nickSplit txt
    allNicks = HashSet.union myNicks nicks
    highlight1 part
      | HashSet.member partId allNicks = coloredIdentifier palette PrivmsgIdentifier myNicks partId
      | otherwise                      = text' defAttr part
      where
        partId = mkId part

-- | Returns image and identifier to be used when collapsing metadata
-- messages.
metadataImg :: IrcSummary -> Maybe (Image', Identifier, Maybe Identifier)
metadataImg msg =
  case msg of
    QuitSummary who     -> Just (char (withForeColor defAttr red   ) 'x', who, Nothing)
    PartSummary who     -> Just (char (withForeColor defAttr red   ) '-', who, Nothing)
    JoinSummary who     -> Just (char (withForeColor defAttr green ) '+', who, Nothing)
    CtcpSummary who     -> Just (char (withForeColor defAttr white ) 'C', who, Nothing)
    NickSummary old new -> Just (char (withForeColor defAttr yellow) '>', old, Just new)
    ChngSummary who     -> Just (char (withForeColor defAttr blue  ) '*', who, Nothing)
    AcctSummary who     -> Just (char (withForeColor defAttr blue  ) '*', who, Nothing)
    _                   -> Nothing

-- | Image used when treating ignored chat messages as metadata
ignoreImage :: Image'
ignoreImage = char (withForeColor defAttr yellow) 'I'


-- | Render the normal view of a chat message line padded and wrapped.
drawWindowLine ::
  Palette     {- ^ palette       -} ->
  Int         {- ^ draw columns  -} ->
  PaddingMode {- ^ nick padding  -} ->
  WindowLine  {- ^ window line   -} ->
  [Image']    {- ^ wrapped lines -}
drawWindowLine palette w padAmt wl = wrap (drawPrefix wl) (view wlImage wl)
  where
    drawTime = timeImage palette . unpackTimeOfDay
    padNick = nickPad padAmt
    wrap pfx body = reverse (lineWrapPrefix w pfx body)
    drawPrefix = views wlTimestamp drawTime <>
                 views wlPrefix    padNick
