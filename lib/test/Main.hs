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
import           Data.Hashable
import           Data.Semigroup
import           Irc.RawIrcMsg
import           Irc.UserInfo
import           Irc.Identifier
import           System.Exit
import           Test.HUnit
import           Text.Read

main :: IO a
main =
  do outcome <- runTestTT tests
     if errors outcome == 0 && failures outcome == 0
       then exitSuccess
       else exitFailure

tests :: Test
tests = test [ irc0, irc2, irc15, ircWithPrefix, ircWithTags,
               parseUserInfos, renderUserInfos, renderIrc,
               badRawMsgs, userInfoFields, identifierInstances ]

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
            { _msgPrefix = Just (UserInfo "morgan.example.net" "" "") })
      (parseRawIrcMsg ":morgan.example.net 254 glguytest 57555 :channels formed")
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

  , assertEqual "optional key"
      (Just (rawIrcMsg "CMD" [])
            { _msgTags = [TagEntry "this" ""] })
      (parseRawIrcMsg "@this CMD")

  , assertEqual "optional keys"
      (Just (rawIrcMsg "CMD" [])
            { _msgTags = [TagEntry "this" "", TagEntry "that" ""] })
      (parseRawIrcMsg "@this;that CMD")

  ]

renderUserInfos :: Test
renderUserInfos = test

  [ assertEqual "missing user and hostname"
      "glguy"
      (renderUserInfo (UserInfo "glguy" "" ""))

  , assertEqual "example cloak"
      "glguy!~glguy@haskell/developer/glguy"
      (renderUserInfo (UserInfo "glguy" "~glguy" "haskell/developer/glguy"))

  , assertEqual "missing user"
      "glguy@haskell/developer/glguy"
      (renderUserInfo (UserInfo "glguy" "" "haskell/developer/glguy"))

  , assertEqual "missing host"
      "glguy!~glguy"
      (renderUserInfo (UserInfo "glguy" "~glguy" ""))

  , assertEqual "extra @ goes into host"
      "nick!user@server@name"
      (renderUserInfo (UserInfo "nick" "user" "server@name"))

  , assertEqual "servername in nick"
      "morgan.example.net"
      (renderUserInfo (UserInfo "morgan.example.net" "" ""))
  ]

userInfoFields :: Test
userInfoFields = test
  [ assertEqual "nickfield" "nick" (userNick (UserInfo "nick" "user" "host"))
  , assertEqual "userfield" "user" (userName (UserInfo "nick" "user" "host"))
  , assertEqual "hostfield" "host" (userHost (UserInfo "nick" "user" "host"))
  ]

parseUserInfos :: Test
parseUserInfos = test

  [ assertEqual "missing user and hostname"
      (UserInfo "glguy" "" "")
      (parseUserInfo "glguy")

  , assertEqual "example cloak"
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
      (UserInfo "morgan.example.net" "" "")
      (parseUserInfo "morgan.example.net")
  ]

renderIrc :: Test
renderIrc = test
  [ assertEqual ""
      ":morgan.example.net 254 glguytest 57555 :channels formed\r\n"
      (renderRawIrcMsg
          (rawIrcMsg "254" ["glguytest", "57555", "channels formed"])
          { _msgPrefix = Just (UserInfo "morgan.example.net" "" "") })

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
            { _msgTags = [TagEntry "time" "value",
                          TagEntry "this" "\n\rand\\ ;that"] })

  , assertEqual "empty tags"
      "@time;magic CMD\r\n"
      (renderRawIrcMsg (rawIrcMsg "CMD" [])
            { _msgTags = [TagEntry "time" "",
                          TagEntry "magic" ""] })

  , assertEqual "empty last argument 1"
      "CMD :\r\n"
      (renderRawIrcMsg (rawIrcMsg "CMD" [""]))

  , assertEqual "empty last argument 2"
      "CMD X :\r\n"
      (renderRawIrcMsg (rawIrcMsg "CMD" ["X", ""]))

  ]

badRawMsgs :: Test
badRawMsgs = test
  [ assertEqual "bad prefix"
      Nothing
      (parseRawIrcMsg ": CMD")
  , assertEqual "empty string"
      Nothing
      (parseRawIrcMsg "")
  , assertEqual "whitespace"
      Nothing
      (parseRawIrcMsg "   ")
  , assertEqual "only prefix"
      Nothing
      (parseRawIrcMsg ":glguy!glguy@glguy")
  , assertEqual "only tags"
      Nothing
      (parseRawIrcMsg "@glguy=tester")
  ]

identifierInstances :: Test
identifierInstances = test
  [ assertEqual "read" (Just ("GLGUY"::Identifier)) (readMaybe "\"glguy\"")
  , assertEqual "read2" (Just ("Glguy"::Identifier)) (readMaybe "\"glguy\"")
  , assertEqual "show1" "\"GLguy\"" (show ("GLguy"::Identifier))
  , assertEqual "show2" "\"glguy\"" (show ("glguy"::Identifier))
  , assertBool "hash"  $ hash ("glguy"::Identifier) ==
                         hash ("GLGUY"::Identifier)
  , assertBool "lt1"   $ "glguy"  < ("tester" :: Identifier)
  , assertBool "gt1"   $ "tester" > ("glguy" :: Identifier)
  , assertBool "lt2"   $ "GLGUY"  < ("tester" :: Identifier)
  , assertBool "gt2"   $ "TESTER" > ("glguy" :: Identifier)
  , assertBool "pre"   $ idPrefix "gl" "GLGUY"
  , assertBool "pre"   $ not $ idPrefix "glguy" "gl"
  ]
