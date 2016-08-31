{-# Language GADTs #-}
{-|
Module      : Main
Description : Tests for the glirc library
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module test various components of glirc

-}
module Main (main) where

import           Client.Commands.Arguments
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
       (parseArguments NoArg "")
  , assertEqual "no arg white"
       (Just ())
       (parseArguments NoArg "   ")
  , assertEqual "no arg fail"
       Nothing
       (parseArguments NoArg " a")

  , assertEqual "required"
       (Just ("argument",()))
       (parseArguments (ReqTokenArg "field" NoArg) " argument ")

  , assertEqual "optional 1"
       (Just (Just ("argument",())))
       (parseArguments (OptTokenArg "field" NoArg) " argument ")

  , assertEqual "optional 2"
       (Just Nothing)
       (parseArguments (OptTokenArg "field" NoArg) "  ")

  , assertEqual "remaining"
       (Just "some stuff ")
       (parseArguments (RemainingArg "field") " some stuff ")
  ]
