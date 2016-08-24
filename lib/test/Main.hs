{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Tests for the irc-core library
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module test IRC message parsing.

-}
module Main (main) where

import qualified Data.Text as Text
import           Data.Semigroup
import           Irc.RawIrcMsg
import           Irc.UserInfo
import           System.Exit
import           Test.HUnit

main :: IO a
main =
  do counts <- runTestTT tests
     if errors counts == 0 && failures counts == 0
       then exitSuccess
       else exitFailure

tests :: Test
tests = test [ irc0, irc2, irc15, ircWithPrefix, ircWithTags, userInfos, renderIrc ]

-- | Check that we can handle commands without parameters
irc0 :: Test
irc0 = test [ assertEqual "" goal (parseRawIrcMsg alt) | alt <- alternatives ]
  where
    goal = Just (rawIrcMsg "COMMAND" [])
    alternatives =
      [ "COMMAND"
      , "COMMAND "
      , "COMMAND  "
      ]

-- | Check that we can handle commands with two parameters and an assortment of spacing
irc2 :: Test
irc2 = test [ assertEqual "" goal (parseRawIrcMsg alt) | alt <- alternatives ]
  where
    goal = Just (rawIrcMsg "COMMAND" ["param1","param2"])
    alternatives =
      [ "COMMAND param1 param2"
      , "COMMAND   param1 param2"
      , "COMMAND   param1   param2"
      , "COMMAND param1   param2"
      , "COMMAND param1   param2   "
      , "COMMAND param1   :param2"
      , "COMMAND param1 :param2"
      ]

-- | Check that we max out at 15 parameters
irc15 :: Test
irc15 = test
  [ assertEqual "" goal (parseRawIrcMsg raw1)
  , assertEqual "" goal (parseRawIrcMsg raw2)
  ]
  where
    goal   = Just (rawIrcMsg "001" (params ++ ["last  two"]))
    params = map (Text.pack . show) [1 .. 14 :: Int]
    raw1 = "001 " <> Text.unwords params <> " last  two"
    raw2 = "001 " <> Text.unwords params <> "  :last  two"

ircWithPrefix :: Test
ircWithPrefix = test
  [ assertEqual ""
      (Just (rawIrcMsg "254" ["glguytest", "57555", "channels formed"])
            { _msgPrefix = Just (UserInfo "morgan.freenode.net" "" "") })
      (parseRawIrcMsg ":morgan.freenode.net 254 glguytest 57555 :channels formed")
  ]

ircWithTags :: Test
ircWithTags = test
  [ assertEqual "without prefix"
      (Just (rawIrcMsg "CMD" [])
            { _msgTags = [TagEntry "time" "value"] })
      (parseRawIrcMsg "@time=value CMD")

  , assertEqual "with prefix"
      (Just (rawIrcMsg "CMD" [])
            { _msgTags = [TagEntry "time" "value"]
            , _msgPrefix = Just (UserInfo "prefix" "user" "host") })
      (parseRawIrcMsg "@time=value :prefix!user@host CMD")

  , assertEqual "two tags"
      (Just (rawIrcMsg "CMD" [])
            { _msgTags = [TagEntry "time" "value", TagEntry "this" "\n\rand\\ ;that"] })
      (parseRawIrcMsg "@time=value;this=\\n\\rand\\\\\\s\\:that CMD")

  , assertEqual "don't escape keys"
      (Just (rawIrcMsg "CMD" [])
            { _msgTags = [TagEntry "this\\s" "value"] })
      (parseRawIrcMsg "@this\\s=value CMD")

  ]

userInfos :: Test
userInfos = test

  [ assertEqual "missing user and hostname"
      (UserInfo "glguy" "" "")
      (parseUserInfo "glguy")

  , assertEqual "freenode cloak"
      (UserInfo "glguy" "~glguy" "haskell/developer/glguy")
      (parseUserInfo "glguy!~glguy@haskell/developer/glguy")

  , assertEqual "missing user"
      (UserInfo "glguy" "" "haskell/developer/glguy")
      (parseUserInfo "glguy@haskell/developer/glguy")

  , assertEqual "missing host"
      (UserInfo "glguy" "~glguy" "")
      (parseUserInfo "glguy!~glguy")

  , assertEqual "extra @ goes into host"
      (UserInfo "nick" "user" "server@name")
      (parseUserInfo "nick!user@server@name")

  , assertEqual "servername in nick"
      (UserInfo "morgan.freenode.net" "" "")
      (parseUserInfo "morgan.freenode.net")
  ]

renderIrc :: Test
renderIrc = test
  [ assertEqual ""
      ":morgan.freenode.net 254 glguytest 57555 :channels formed\r\n"
      (renderRawIrcMsg
          (rawIrcMsg "254" ["glguytest", "57555", "channels formed"])
          { _msgPrefix = Just (UserInfo "morgan.freenode.net" "" "") })

  , assertEqual ""
      "254 glguytest 57555 :channels formed\r\n"
      (renderRawIrcMsg
          (rawIrcMsg "254" ["glguytest", "57555", "channels formed"]))

  , assertEqual ""
      "CMD param:with:colon\r\n"
      (renderRawIrcMsg (rawIrcMsg "CMD" ["param:with:colon"]))

  , assertEqual ""
      "CMD ::param\r\n"
      (renderRawIrcMsg (rawIrcMsg "CMD" [":param"]))

  , assertEqual ""
      "CMD\r\n"
      (renderRawIrcMsg (rawIrcMsg "CMD" []))

  , assertEqual "two tags"
      "@time=value;this=\\n\\rand\\\\\\s\\:that CMD\r\n"
      (renderRawIrcMsg (rawIrcMsg "CMD" [])
            { _msgTags = [TagEntry "time" "value", TagEntry "this" "\n\rand\\ ;that"] })

  ]
