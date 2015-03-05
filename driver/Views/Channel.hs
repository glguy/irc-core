{-# LANGUAGE OverloadedStrings #-}
module Views.Channel where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Monoid
import Data.CaseInsensitive (CI)
import Data.Char (isControl)
import Data.Foldable (toList)
import Data.List (stripPrefix, intersperse)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime, formatTime)
import Graphics.Vty.Image
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text

import Irc.Format
import Irc.Model

import ClientState
import ImageUtils

detailedImageForState :: ClientState -> [Image]
detailedImageForState st
  = map renderOne
  $ activeMessages st
  where
  width = view clientWidth st
  renderOne x =
      renderTimestamp (view mesgStamp x) <|>
      string (withForeColor defAttr blue) (ty ++ " ") <|>
      renderFullUsermask (view mesgSender x) <|>
      string (withForeColor defAttr blue) (": ") <|>
      cleanText content
    where
    (ty, content) = case view mesgType x of
       JoinMsgType -> ("J", "")
       PartMsgType txt -> ("P", txt)
       NickMsgType txt -> ("C", asUtf8 (idBytes txt))
       QuitMsgType txt -> ("Q", txt)
       PrivMsgType txt -> ("M", txt)
       TopicMsgType txt -> ("T", txt)
       ActionMsgType txt -> ("A", txt)
       NoticeMsgType txt -> ("N", txt)
       KickMsgType who txt -> ("K", asUtf8 (idBytes who) <> " - " <> txt)
       ErrorMsgType txt -> ("E", txt)
       ModeMsgType pol mode arg -> ("Z", (if pol then "+" else "-")
                                        <> Text.pack [mode, ' ']
                                        <> asUtf8 arg)

renderTimestamp :: UTCTime -> Image
renderTimestamp
  = string (withForeColor defAttr brightBlack)
  . formatTime defaultTimeLocale "%H:%M:%S "

activeMessages :: ClientState -> [IrcMessage]
activeMessages st =
  case stripPrefix "/filter " (clientInput st) of
    Nothing -> toList msgs
    Just nick -> filter (nickFilter (BS8.pack nick)) (toList msgs)
  where
  msgs = view (clientMessages . ix (focusedName st) . mlMessages) st
  conn = view clientConnection st
  nickFilter nick msg
    = views mesgSender userNick msg == mkId nick

compressedImageForState :: ClientState -> [Image]
compressedImageForState st
  = map renderOne
  $ compressMessages (view clientIgnores st)
  $ activeMessages st
  where
  width = view clientWidth st

  formatNick me = identImg (withForeColor defAttr color)
    where
    color
      | me        = green
      | otherwise = yellow

  renderOne (CompChat modes me who what) =
      modePrefix modes <|>
      formatNick me who <|>
      string (withForeColor defAttr blue) (": ") <|>
      cleanText what

  renderOne (CompNotice modes who what) =
      modePrefix modes <|>
      identImg (withForeColor defAttr red) who <|>
      string (withForeColor defAttr blue) (": ") <|>
      cleanText what

  renderOne (CompAction modes who what) =
      modePrefix modes <|>
      identImg (withForeColor defAttr blue) who <|>
      char defAttr ' ' <|>
      cleanText what

  renderOne (CompKick op who reason) =
      identImg (withForeColor defAttr yellow) op <|>
      string (withForeColor defAttr red) " kicked " <|>
      identImg (withForeColor defAttr yellow) who <|>
      string (withForeColor defAttr blue) (": ") <|>
      cleanText reason

  renderOne (CompError err) =
      string (withForeColor defAttr red) "Error: " <|>
      cleanText err

  renderOne (CompMode who pol m arg) =
      identImg (withForeColor defAttr yellow) who <|>
      string (withForeColor defAttr red) " set mode " <|>
      string (withForeColor defAttr white) ((if pol then '+' else '-'):[m,' ']) <|>
      utf8Bytestring' (withForeColor defAttr yellow) arg

  renderOne (CompTopic who txt)
    = identImg (withForeColor defAttr yellow) who <|>
      string (withForeColor defAttr red) " set topic " <|>
      cleanText txt

  renderOne (CompMeta xs) =
      cropRight width (horizCat (intersperse (char defAttr ' ') (map renderMeta xs)))

  renderMeta (CompJoin who)
    =   char (withForeColor defAttr green) '+'
    <|> identImg defAttr who
  renderMeta (CompPart who)
    =   char (withForeColor defAttr red) '-'
    <|> identImg defAttr who
  renderMeta (CompQuit who)
    =   char (withForeColor defAttr red) 'x'
    <|> identImg defAttr who
  renderMeta (CompNick who who')
    =   identImg defAttr who
    <|> char (withForeColor defAttr yellow) '-'
    <|> identImg defAttr who'
  renderMeta (CompIgnored who)
    =   char (withForeColor defAttr brightBlack) 'I'
    <|> identImg defAttr who


  conn = view clientConnection st

  prefixes = view (connChanModes . modesPrefixModes) conn

  modePrefix modes =
    string (withForeColor defAttr blue)
           (mapMaybe (`lookup` prefixes) modes)

compressMessages :: Set Identifier -> [IrcMessage] -> [CompressedMessage]
compressMessages _ [] = []
compressMessages ignores (x:xs) =
  case view mesgType x of
    NoticeMsgType txt | visible ->
        CompNotice (view mesgModes x) nick txt
      : compressMessages ignores xs
    PrivMsgType txt | visible ->
        CompChat (view mesgModes x) (view mesgMe x) nick txt
      : compressMessages ignores xs
    ActionMsgType txt | visible ->
        CompAction (view mesgModes x) nick txt
      : compressMessages ignores xs
    KickMsgType u reason -> CompKick nick u reason
                       : compressMessages ignores xs
    ErrorMsgType err  -> CompError err
                       : compressMessages ignores xs
    ModeMsgType pol mode arg -> CompMode nick pol mode arg
                       : compressMessages ignores xs
    TopicMsgType txt  -> CompTopic nick txt
                       : compressMessages ignores xs
    _                 -> meta ignores [] (x:xs)

  where
  nick = views mesgSender userNick x
  visible = not (view (contains nick) ignores)

meta :: Set Identifier -> [CompressedMeta] -> [IrcMessage] -> [CompressedMessage]
meta _ acc [] = [CompMeta (reverse acc)]
meta ignores acc (x:xs) =
    case view mesgType x of
      JoinMsgType -> meta ignores (CompJoin nick : acc) xs
      QuitMsgType{} -> meta ignores (CompQuit nick : acc) xs
      PartMsgType{} -> meta ignores (CompPart nick : acc) xs
      NickMsgType nick' -> meta ignores (CompNick nick nick' : acc) xs
      PrivMsgType{} | ignored -> meta ignores (CompIgnored nick : acc) xs
      ActionMsgType{} | ignored -> meta ignores (CompIgnored nick : acc) xs
      NoticeMsgType{} | ignored -> meta ignores (CompIgnored nick : acc) xs
      _ -> CompMeta (reverse acc) : compressMessages ignores (x:xs)

  where
  nick = views mesgSender userNick x
  ignored = view (contains nick) ignores

data CompressedMessage
  = CompChat String Bool Identifier Text
  | CompNotice String Identifier Text
  | CompAction String Identifier Text
  | CompKick Identifier Identifier Text
  | CompError Text
  | CompMode Identifier Bool Char ByteString
  | CompTopic Identifier Text
  | CompMeta [CompressedMeta]

data CompressedMeta
  = CompJoin Identifier
  | CompQuit Identifier
  | CompPart Identifier
  | CompNick Identifier Identifier
  | CompIgnored Identifier
