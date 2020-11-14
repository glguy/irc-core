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
  , Highlight(..)
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
import qualified Data.HashMap.Strict as HashMap
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
  , rendHighlights :: HashMap Identifier Highlight -- ^ words to highlight
  , rendPalette    :: Palette -- ^ nick color palette
  , rendAccounts   :: Maybe (HashMap Identifier UserAndHost)
  }

-- | Default 'MessageRendererParams' with no sigils or nicknames specified
defaultRenderParams :: MessageRendererParams
defaultRenderParams = MessageRendererParams
  { rendStatusMsg   = ""
  , rendUserSigils  = ""
  , rendHighlights  = HashMap.empty
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

ctxt :: Text -> Image'
ctxt = text' defAttr . cleanText

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
      hilites = rendHighlights rp
      rm      = NormalRender

      who n   = string (view palSigil pal) sigils <> ui
        where
          baseUI    = coloredUserInfo pal rm hilites n
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
                       | otherwise -> baseUI <> "(" <> ctxt acct <> ")"
                     Nothing -> "~" <> baseUI
  in
  case body of
    Join       {} -> mempty
    Part       {} -> mempty
    Quit       {} -> mempty
    Ping       {} -> mempty
    Pong       {} -> mempty
    Nick       {} -> mempty

    -- details in message part
    Topic src _ _  -> who src
    Kick src _ _ _ -> who src
    Mode src _ _   -> who src
    Invite src _ _ -> who src

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
      hilites = rendHighlights rp
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
    Topic _ _ txt ->
      "changed the topic: " <>
      parseIrcTextWithNicks pal hilites False txt

    Kick _who _channel kickee reason ->
      "kicked " <>
      coloredIdentifier pal NormalIdentifier hilites kickee <>
      ": " <>
      parseIrcTextWithNicks pal hilites False reason

    Notice     _ _          txt -> parseIrcTextWithNicks pal hilites False txt
    Privmsg    _ _          txt -> parseIrcTextWithNicks pal hilites False txt
    Wallops    _            txt -> parseIrcTextWithNicks pal hilites False txt
    Ctcp       _ _ "ACTION" txt -> parseIrcTextWithNicks pal hilites False txt
    Ctcp {}                     -> mempty
    CtcpNotice _ _ cmd      txt -> parseIrcText cmd <> " " <>
                                   parseIrcTextWithNicks pal hilites False txt

    Reply code params -> renderReplyCode pal NormalRender code params
    UnknownMsg irc ->
      ctxt (view msgCommand irc) <>
      char defAttr ' ' <>
      separatedParams (view msgParams irc)
    Cap cmd           -> ctxt (capCmdText cmd)

    Mode _ _ params ->
      "set mode: " <>
      ircWords params

    Invite _ tgt chan ->
      "invited " <>
      coloredIdentifier pal NormalIdentifier hilites tgt <>
      " to " <> ctxt (idText chan)

    Account _ acct -> if Text.null acct then "*" else ctxt acct
    Chghost _ user host -> ctxt user <> " " <> ctxt host

-- | Render a chat message given a rendering mode, the sigils of the user
-- who sent the message, and a list of nicknames to highlight.
fullIrcLineImage ::
  MessageRendererParams ->
  IrcMsg -> Image'
fullIrcLineImage !rp body =
  let quietAttr = view palMeta pal
      pal     = rendPalette rp
      sigils  = rendUserSigils rp
      hilites = rendHighlights rp
      rm      = DetailedRender

      plainWho = coloredUserInfo pal rm hilites

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
      coloredIdentifier pal NormalIdentifier hilites new

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
      coloredIdentifier pal NormalIdentifier hilites kickee <>
      ": " <>
      parseIrcText reason

    Topic src _dst txt ->
      string quietAttr "tpic " <>
      who src <>
      " changed the topic: " <>
      parseIrcText txt

    Invite src tgt chan ->
      string quietAttr "invt " <>
      who src <>
      " invited " <>
      coloredIdentifier pal NormalIdentifier hilites tgt <>
      " to " <>
      ctxt (idText chan)

    Notice src _dst txt ->
      string quietAttr "note " <>
      who src <>
      string (withForeColor defAttr red) ": " <>
      parseIrcTextWithNicks pal hilites False txt

    Privmsg src _dst txt ->
      string quietAttr "chat " <>
      who src <> ": " <>
      parseIrcTextWithNicks pal hilites False txt

    Wallops src txt ->
      string quietAttr "wall " <>
      who src <> ": " <>
      parseIrcTextWithNicks pal hilites False txt

    Ctcp src _dst "ACTION" txt ->
      string quietAttr "actp " <>
      string (withForeColor defAttr blue) "* " <>
      who src <> " " <>
      parseIrcTextWithNicks pal hilites False txt

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
      foldMap (\ui -> coloredUserInfo pal rm hilites ui <> char defAttr ' ')
        (view msgPrefix irc) <>
      ctxt (view msgCommand irc) <>
      char defAttr ' ' <>
      separatedParams (view msgParams irc)

    Cap cmd ->
      text' (withForeColor defAttr magenta) (renderCapCmd cmd) <>
      ": " <>
      ctxt (capCmdText cmd)

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
      if Text.null acct then "*" else ctxt acct

    Chghost user newuser newhost ->
      string quietAttr "chng " <>
      who user <> ": " <>
      ctxt newuser <> " " <> ctxt newhost


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
replyCodePrefix code = text' attr (replyCodeText info) <> ":"
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
        RPL_STATSPLINE   -> portParamsImage
        RPL_STATSILINE   -> authLineParamsImage
        RPL_STATSDLINE   -> dlineParamsImage
        RPL_STATSQLINE   -> banlineParamsImage "Q"
        RPL_STATSXLINE   -> banlineParamsImage "X"
        RPL_STATSKLINE   -> klineParamsImage
        RPL_STATSCLINE   -> connectLineParamsImage
        RPL_STATSHLINE   -> hubLineParamsImage
        RPL_STATSCOMMANDS-> commandsParamsImage
        RPL_STATSOLINE   -> operLineParamsImage
        RPL_STATSULINE   -> sharedLineParamsImage
        RPL_STATSYLINE   -> classLineParamsImage
        RPL_STATSDEBUG   -> statsDebugParamsImage
        RPL_HELPSTART    -> statsDebugParamsImage
        RPL_HELPTXT      -> statsDebugParamsImage
        RPL_TESTMASKGECOS-> testmaskGecosParamsImage
        RPL_LOCALUSERS   -> lusersParamsImage
        RPL_GLOBALUSERS  -> lusersParamsImage
        RPL_LUSEROP      -> params_2_3_Image
        RPL_LUSERCHANNELS-> params_2_3_Image
        RPL_ENDOFSTATS   -> params_2_3_Image
        RPL_AWAY         -> awayParamsImage
        RPL_ETRACE       -> etraceParamsImage
        RPL_ETRACEFULL   -> etraceFullParamsImage
        RPL_ENDOFTRACE   -> params_2_3_Image
        RPL_ENDOFHELP    -> params_2_3_Image

        ERR_NOPRIVS      -> params_2_3_Image
        ERR_HELPNOTFOUND -> params_2_3_Image
        ERR_NEEDMOREPARAMS -> params_2_3_Image
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
        [_, p, _] -> ctxt p
        _         -> rawParamsImage

    param_3_3_Image =
      case params of
        [_, _, txt] -> ctxt txt
        _           -> rawParamsImage

    param_3_4_Image =
      case params of
        [_, _, p, _] -> ctxt p
        _            -> rawParamsImage

    topicWhoTimeParamsImage =
      case params of
        [_, _, who, time] ->
          label "set by" <>
          ctxt who <>
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
          text' (withStyle defAttr bold) (cleanText nick) <>
          text' (view palLabel pal) "!" <>
          ctxt user <>
          text' (view palLabel pal) "@" <>
          ctxt host <>
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
          ctxt name <>
          label " duration" <> string defAttr (prettyTime 60 (Text.unpack mins)) <>
          label " mask"     <> ctxt mask <>
          label " reason"   <> ctxt msg
        _ -> rawParamsImage

    linkInfoParamsImage =
      case params of
        [_, name, sendQ, sendM, sendK, recvM, recvK, Text.words -> conn : idle : caps] ->
          ctxt name <>
          label " sendQ" <> ctxt sendQ <>
          label " sendM" <> ctxt sendM <>
          label " sendK" <> ctxt sendK <>
          label " recvM" <> ctxt recvM <>
          label " recvK" <> ctxt recvK <>
          label " since" <> string defAttr (prettyTime 1 (Text.unpack conn)) <>
          label " idle"  <> string defAttr (prettyTime 1 (Text.unpack idle)) <>
          label " caps"  <> ctxt (Text.unwords caps)
        _ -> rawParamsImage

    authLineParamsImage =
      case params of
        [_, "I", name, pass, mask, port, klass] ->
          ctxt name <>
          (if pass == "<NULL>" then mempty else label " pass" <> ctxt pass) <>
          label " mask" <> ctxt mask' <>
          (if port == "0" then mempty else label " port" <> ctxt port) <>
          label " class" <> ctxt klass <>
          (if null special then mempty else
            label " special" <> ctxt (Text.unwords special))
          where
            (mask', special) = parseILinePrefix mask
        _ -> rawParamsImage

    banlineParamsImage expect =
      case params of
        [_, letter, hits, mask, reason] | letter == expect ->
          ctxt mask <>
          label " reason" <> ctxt reason <>
          label " hits" <> ctxt hits
        _ -> rawParamsImage

    testmaskGecosParamsImage =
      case params of
        [_, local, remote, mask, gecos, _txt] ->
          ctxt mask <>
          (if gecos == "*" then mempty else label " gecos" <> ctxt gecos) <>
          label " local"  <> ctxt local <>
          label " remote" <> ctxt remote
        _ -> rawParamsImage

    portParamsImage =
      case params of
        [_, "P", port, host, count, flags] ->
          ctxt port <>
          label " host"  <> ctxt host <>
          label " count" <> ctxt count <>
          label " flags" <> ctxt flags
        _ -> rawParamsImage

    dlineParamsImage =
      case params of
        [_, flag, host, reason] ->
          ctxt flag <>
          label " host:" <> ctxt host <>
          label " reason" <> ctxt reason
        _ -> rawParamsImage

    klineParamsImage =
      case params of
        [_, flag, host, "*", user, reason] ->
          ctxt flag <>
          label " host"  <> ctxt host <>
          (if user == "*" then mempty else label " user" <> ctxt user) <>
          label " reason" <> ctxt reason
        _ -> rawParamsImage

    statsDebugParamsImage =
      case params of
        [_, flag, txt] -> ctxt flag <> label " txt"  <> ctxt txt
        _ -> rawParamsImage

    lusersParamsImage =
      case params of
        [_, n, m, _txt] -> ctxt n <> label " max"  <> ctxt m
        _ -> rawParamsImage

    connectLineParamsImage =
      case params of
        [_, "C", mask, flagTxt, host, port, klass, certfp] ->
          ctxt mask <>
          label " host" <> ctxt host <>
          label " port" <> ctxt port <>
          label " class" <> ctxt klass <>
          (if certfp == "*" then mempty else label " certfp" <> ctxt certfp) <>
          (if null flags then mempty else label " flags" <> ctxt (Text.unwords flags))
          where
            flags = parseCLineFlags flagTxt
        [_, "C", mask, flagTxt, host, port, klass] ->
          ctxt mask <>
          label " host" <> ctxt host <>
          label " port" <> ctxt port <>
          label " class" <> ctxt klass <>
          (if null flags then mempty else label " flags" <> ctxt (Text.unwords flags))
          where
            flags = parseCLineFlags flagTxt
        _ -> rawParamsImage

    hubLineParamsImage =
      case params of
        [_, "H", host, "*", server, "0", "-1"] ->
          ctxt host <> label " server" <> ctxt server
        [_, "H", host, "*", server] ->
          ctxt host <> label " server" <> ctxt server
        _ -> rawParamsImage

    commandsParamsImage =
      case params of
        [_, cmd, count, bytes, rcount] ->
          ctxt cmd <>
          label " count" <> ctxt count <>
          label " bytes" <> ctxt bytes <>
          label " remote-count" <> ctxt rcount
        _ -> rawParamsImage

    operLineParamsImage =
      case params of
        [_, "O", mask, host, name, privset, "-1"] ->
          ctxt mask <>
          label " host" <> ctxt host <>
          label " name" <> ctxt name <>
          (if privset == "0" then mempty else label " privset" <> ctxt privset)
        _ -> rawParamsImage

    sharedLineParamsImage =
      case params of
        [_, "U", server, mask, flags] ->
          ctxt server <>
          label " mask" <> ctxt mask <>
          label " flags" <> ctxt flags
        _ -> rawParamsImage

    classLineParamsImage =
      case params of
        [_, "Y", name, pingFreq, conFreq, maxUsers, maxSendq, maxLocal, maxGlobal, curUsers] ->
          ctxt name <>
          label " ping-freq" <> ctxt pingFreq <>
          label " con-freq" <> ctxt conFreq <>
          label " max-users" <> ctxt maxUsers <>
          label " max-sendq" <> ctxt maxSendq <>
          label " max-local" <> ctxt maxLocal <>
          label " max-global" <> ctxt maxGlobal <>
          label " current" <> ctxt curUsers
        _ -> rawParamsImage

    awayParamsImage =
      case params of
        [_, nick, txt] -> ctxt nick <> label " msg" <> parseIrcText txt
        _ -> rawParamsImage

    etraceParamsImage =
      case params of
        [_, kind, server, nick, user, host, ip, gecos] ->
          ctxt nick <> "!" <> ctxt user <> "@" <> ctxt host <>
          label " gecos" <> ctxt gecos <>
          (if ip == "0" || ip == "255.255.255.255" then mempty else label " ip" <> ctxt ip) <>
          label " server" <> ctxt server <>
          label " kind" <> ctxt kind
        _ -> rawParamsImage

    etraceFullParamsImage =
      case params of
        [_, kind, klass, nick, user, host, ip, p1, p2, gecos] ->
          ctxt nick <> "!" <> ctxt user <> "@" <> ctxt host <>
          label " gecos" <> ctxt gecos <>
          (if ip == "0" || ip == "255.255.255.255" then mempty else label " ip" <> ctxt ip) <>
          label " kind" <> ctxt kind <>
          label " class" <> ctxt klass <>
          label " p1" <> ctxt p1 <>
          label " p2" <> ctxt p2
        _ -> rawParamsImage

parseCLineFlags :: Text -> [Text]
parseCLineFlags = go []
  where
    go acc xs =
      case Text.uncons xs of
        Just (x, xs') ->
          case getFlag x of
            Nothing   -> go (Text.singleton x:acc) xs'
            Just flag -> go (flag:acc) xs'
        Nothing -> reverse acc

    getFlag x =
      case x of
        'A' -> Just "auto-connect"
        'M' -> Just "sctp"
        'S' -> Just "tls"
        'T' -> Just "topic-burst"
        'Z' -> Just "compressed"
        _   -> Nothing

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
  HashMap Identifier Highlight {- ^ highlights -} ->
  Identifier          {- ^ identifier to draw -} ->
  Image'
coloredIdentifier palette icm hilites ident =
  text' color (cleanText (idText ident))
  where
    color
      | Just HighlightMe == HashMap.lookup ident hilites =
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
  HashMap Identifier Highlight {- ^ highlights -} ->
  UserInfo           {- ^ userinfo to draw-} ->
  Image'
coloredUserInfo palette NormalRender hilites ui =
  coloredIdentifier palette NormalIdentifier hilites (userNick ui)
coloredUserInfo palette DetailedRender hilites !ui =
  mconcat
    [ coloredIdentifier palette NormalIdentifier hilites (userNick ui)
    , aux '!' (userName ui)
    , aux '@' (userHost ui)
    ]
  where
    quietAttr = view palMeta palette
    aux x xs
      | Text.null xs = mempty
      | otherwise    = char quietAttr x <> text' quietAttr (cleanText xs)

-- | Render an identifier without using colors. This is useful for metadata.
quietIdentifier :: Palette -> Identifier -> Image'
quietIdentifier palette ident =
  text' (view palMeta palette) (cleanText (idText ident))

data Highlight
  = HighlightMe
  | HighlightNick
  | HighlightError
  deriving Eq

-- | Parse message text to construct an image. If the text has formatting
-- control characters in it then the text will be rendered according to
-- the formatting codes. Otherwise the nicknames in the message are
-- highlighted.
parseIrcTextWithNicks ::
  Palette            {- ^ palette      -} ->
  HashMap Identifier Highlight {- ^ Highlights -} ->
  Bool               {- ^ explicit controls rendering -} ->
  Text               {- ^ input text   -} ->
  Image'             {- ^ colored text -}
parseIrcTextWithNicks palette hilite explicit txt
  | Text.any isControl txt = parseIrcText' explicit txt
  | otherwise              = highlightNicks palette hilite txt

-- | Given a list of nicknames and a chat message, this will generate
-- an image where all of the occurrences of those nicknames are colored.
highlightNicks ::
  Palette ->
  HashMap Identifier Highlight {- ^ highlights -} ->
  Text -> Image'
highlightNicks palette hilites txt = foldMap highlight1 txtParts
  where
    txtParts = nickSplit txt
    highlight1 part =
      case HashMap.lookup partId hilites of
        Nothing -> ctxt part
        Just HighlightError -> text' (view palError palette) part
        _ -> coloredIdentifier palette PrivmsgIdentifier hilites partId
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
