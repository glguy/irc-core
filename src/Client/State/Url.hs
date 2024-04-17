{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Client.State.Url
Description : Function for extracting URLs from text
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

-}

module Client.State.Url
  ( UrlPair
  , urlList
  ) where

import           Client.Message (summaryActor)
import           Client.State
import           Client.State.Focus (Subfocus(..), focusNetwork, Focus, actualFocus)
import           Client.State.Network
import           Client.State.Window
import           Client.WhoReply
import           Control.Lens
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import           Irc.Identifier (Identifier)
import           Irc.UserInfo (UserInfo(..))
import           Text.Regex.TDFA
import           Text.Regex.TDFA.ByteString (compile)

-- | A URL and identifiers of those who provided that URL.
type UrlPair = (Text, [Identifier])

-- | Regular expression for matching HTTP/HTTPS URLs in chat text.
urlPattern :: Regex
Right urlPattern =
  compile
    defaultCompOpt
    defaultExecOpt{captureGroups=False}
    "https?://([[:alnum:]-]+\\.)*([[:alnum:]-]+)(:[[:digit:]]+)?(/[-0-9a-zA-Z$_.+!*'(),%?&=:@/;~#]*)?|\
    \<https?://[^>]*>|\
    \\\(https?://[^\\)]*\\)"

-- | Find all the URL matches using 'urlPattern' in a given 'Text' suitable
-- for being opened. Surrounding @<@ and @>@ are removed.
urlMatches :: LText.Text -> [Text]
urlMatches txt = removeBrackets . extractText . (^?! ix 0)
             <$> matchAll urlPattern (LText.unpack txt)
  where
    extractText (off,len) = LText.toStrict
                          $ LText.take (fromIntegral len)
                          $ LText.drop (fromIntegral off) txt

    removeBrackets t =
      case Text.uncons t of
       Just ('<',t') | not (Text.null t') -> Text.init t'
       Just ('(',t') | not (Text.null t') -> Text.init t'
       _                                  -> t

-- | Generate a list of URLs from the current focus and subfocus.
urlList :: ClientState -> [UrlPair]
urlList st = urlDedup $ urlListForFocus focus subfocus st
  where
    focus = view clientFocus st
    subfocus = view clientSubfocus st

urlListForFocus :: Focus -> Subfocus -> ClientState -> [UrlPair]
urlListForFocus focus subfocus st = case (netM, subfocus) of
  (Just cs, FocusChanList _ min' max') ->
    matchesTopic st min' max' cs
  (Just cs, FocusWho _) ->
    matchesWhoReply st cs
  (_, _) ->
    toListOf (clientWindows . ix focus . winMessages . each . folding (matchesMsg st)) st
  where
    netM = do
      net <- focusNetwork $ actualFocus subfocus focus
      view (clientConnections . at net) st

matchesMsg :: ClientState -> WindowLine -> [UrlPair]
matchesMsg st wl =
  [ (url, maybeToList $ views wlSummary summaryActor wl)
  | url <- concatMap urlMatches $ clientFilter st id [views wlText id wl]
  ]

matchesTopic :: ClientState -> Maybe Int -> Maybe Int -> NetworkState -> [UrlPair]
matchesTopic st min' max' cs =
  [ (url, [chan])
  | (chan, _, topic) <- clientFilterChannels st min' max' $ view (csChannelList . clsItems) cs
  , url <- urlMatches $ LText.fromStrict topic
  ]

matchesWhoReply :: ClientState -> NetworkState -> [UrlPair]
matchesWhoReply st cs =
  [ (url, [userNick $ view whoUserInfo wri])
  | wri <- clientFilter st whoFilterText $ view (csWhoReply . whoItems) cs
  , url <- urlMatches $ LText.fromStrict $ view whoRealname wri
  ]

-- | Deduplicates URLs, combining their identifiers while preserving order.
urlDedup :: [UrlPair] -> [UrlPair]
urlDedup pairs = rebuildList hmap [] pairs
  where
    rebuildList _     pairs' [] = reverse pairs'
    rebuildList hmap' pairs' ((url, _):rest)
      | HashMap.null hmap = reverse pairs'
      | otherwise = case ids of
        Just keys -> rebuildList hmapU ((url, reverse keys):pairs') rest
        Nothing -> rebuildList hmapU pairs' rest
      where
        (ids, hmapU) = HashMap.alterF (\v -> (v, Nothing)) url hmap'
    hmap = HashMap.fromListWith List.union pairs
