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
import           System.Exit
import           Test.HUnit

main :: IO a
main =
  do counts <- runTestTT tests
     if errors counts == 0 && failures counts == 0
       then exitSuccess
       else exitFailure

tests :: Test
tests = test [ irc0, irc2, irc15 ]

irc0 :: Test
irc0 = test [ assertEqual "" goal (parseRawIrcMsg alt) | alt <- alternatives ]
  where
    goal = Just (rawIrcMsg "COMMAND" [])
    alternatives =
      [ "COMMAND"
      , "COMMAND "
      , "COMMAND  "
      ]

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
