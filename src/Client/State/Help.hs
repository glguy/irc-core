{-# Language TemplateHaskell, OverloadedStrings #-}

{-|
Module      : Client.State.Help
Description : Type and utility functions for the help buffer
Copyright   : (c) TheDaemoness, 2024
License     : ISC
Maintainer  : emertens@gmail.com

The help buffer is basically a glorified list of @Image'@s with some additional bookkeeping data.
-}

module Client.State.Help
  (HelpState
  , HelpQuery(..)
  , hsQuery
  , hsImages
  , helpQueryToText
  , makeHelp
  , awaitHelp
  , awaitingHelp
  , applyHelpReply
  ) where

import Client.Image.PackedImage (Image')
import Control.Lens
import Data.Text (Text, append)
import Irc.Codes
import Irc.Message (IrcMsg (Reply))
import Client.Image.MircFormatting (parseIrcText)
import Client.Image.Palette (Palette)

data HelpQuery = HelpList | HelpCmd Text | HelpNet Text Text | HelpNetPartial Text Text (Maybe Text)

helpQueryToText :: HelpQuery -> Maybe Text
helpQueryToText (HelpList)                   = Nothing
helpQueryToText (HelpCmd txt)                = Just txt
helpQueryToText (HelpNet net topic)          = Just (net `append` ":" `append` topic)
helpQueryToText (HelpNetPartial net topic _) = Just (net `append` ":" `append` topic)

-- | Cached help query and rendered help text.
data HelpState = HelpState
  { _hsQuery  :: HelpQuery
  , _hsImages :: [Image']
  }

makeLenses ''HelpState

makeHelp :: Maybe Text -> [Image'] -> HelpState
makeHelp (Just cmd) images = HelpState { _hsQuery = HelpCmd cmd, _hsImages = images }
makeHelp Nothing    images = HelpState { _hsQuery = HelpList,    _hsImages = images }

awaitHelp :: Text -> Text -> HelpState
awaitHelp net query = HelpState { _hsQuery = HelpNetPartial net query Nothing, _hsImages = [] }

awaitingHelp :: HelpState -> Maybe Text
awaitingHelp hs = case _hsQuery hs of
  HelpNetPartial net _ _ -> Just net
  _                      -> Nothing

applyHelpReply :: Palette -> IrcMsg -> HelpState -> HelpState
applyHelpReply pal irc hs = case (irc, _hsQuery hs) of
  (Reply _ RPL_HELPSTART (_:rtopic':text:_), HelpNetPartial net topic Nothing) ->
    HelpState
       { _hsQuery = HelpNetPartial net topic (Just rtopic')
       , _hsImages = [parseIrcText pal text]
       }
  (Reply _ RPL_HELPTXT (_:rtopic':text:_), HelpNetPartial _ _ (Just rtopic)) | rtopic' == rtopic ->
    hs { _hsImages = parseIrcText pal text:_hsImages hs }
  (Reply _ RPL_ENDOFHELP (_:rtopic':text:_), HelpNetPartial net topic (Just rtopic)) | rtopic' == rtopic ->
    HelpState
      { _hsQuery = HelpNet net topic
      , _hsImages = parseIrcText pal text:_hsImages hs
      }
  _ -> hs

