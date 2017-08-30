{-# Language GADTs #-}
{-|
Module      : Main
Description : Tests for the glirc library
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module test various components of glirc

-}
module Main (main) where

import           Client.Commands.Arguments.Spec
import           Client.Commands.Arguments.Parser
import           Control.Applicative
import           System.Exit
import           Test.HUnit

main :: IO a
main =
  do outcome <- runTestTT tests
     if errors outcome == 0 && failures outcome == 0
       then exitSuccess
       else exitFailure

tests :: Test
tests = test [ argumentParserTests ]

argumentParserTests :: Test
argumentParserTests = test
  [ assertEqual "no arg empty"
       (Just ())
       (parse () (pure ()) "")
  , assertEqual "no arg white"
       (Just ())
       (parse () (pure ()) "   ")
  , assertEqual "no arg fail"
       Nothing
       (parse () (pure ()) " a")

  , assertEqual "required"
       (Just "argument")
       (parse () (simpleToken "field") " argument ")

  , assertEqual "optional 1"
       (Just (Just "argument"))
       (parse () (optionalArg (simpleToken "field")) " argument ")

  , assertEqual "optional 2"
       (Just Nothing)
       (parse () (optionalArg (simpleToken "field")) "  ")

  , assertEqual "remaining"
       (Just "some stuff ")
       (parse () (remainingArg "field") " some stuff ")

  , assertEqual "token and empty remaining"
       (Just ("some", ""))
       (parse () (liftA2 (,) (simpleToken "first") (remainingArg "second")) " some")

  , assertEqual "token and empty remaining 1"
       (Just ("some", ""))
       (parse () (liftA2 (,) (simpleToken "first") (remainingArg "second")) "  some ")

  , assertEqual "token and remaining"
       (Just ("some", "text here"))
       (parse () (liftA2 (,) (simpleToken "first") (remainingArg "second")) " some text here")

  , assertEqual "extra whitespace token and remaining"
       (Just ("some", " text here"))
       (parse () (liftA2 (,) (simpleToken "first") (remainingArg "second")) "  some  text here")
  ]
