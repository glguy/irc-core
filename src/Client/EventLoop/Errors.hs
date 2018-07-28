{-|
Module      : Client.EventLoop.Errors
Description : Human-readable versions of connection failure
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a prettier rendering for exceptions that are
common in network connects as well as hints about what causes these
errors.
-}

module Client.EventLoop.Errors
  ( exceptionToLines
  ) where

import           Control.Exception
import           Data.List.NonEmpty (NonEmpty(..))
import           OpenSSL.Session
import           Hookup (ConnectionFailure(..))

-- | Compute the message message text to be used for a connection error
exceptionToLines ::
  SomeException   {- ^ network error -} ->
  NonEmpty String {- ^ client lines  -}
exceptionToLines
  = indentMessages
  . exceptionToLines'

indentMessages :: NonEmpty String -> NonEmpty String
indentMessages (x :| xs) = x :| map ("⋯ "++) xs

exceptionToLines' ::
  SomeException   {- ^ network error -} ->
  NonEmpty String {- ^ client lines  -}
exceptionToLines' ex

  -- connection package errors
  | Just err  <- fromException ex = explainHookupError err

  -- HsOpenSSL errors
  | Just _ <- fromException ex :: Maybe ConnectionAbruptlyTerminated =
     "Connection abruptly terminated" :| []
  | Just (ProtocolError e) <- fromException ex =
     ("TLS protocol error: " ++ e) :| []

  -- IOErrors, typically network package.
  | Just ioe <- fromException ex =
     explainIOError ioe :| []

  -- Anything else including glirc's errors (which use displayException)
  | otherwise = displayException ex :| []

explainIOError :: IOError -> String
explainIOError ioe = "IO error: " ++ displayException ioe

explainHookupError :: ConnectionFailure -> NonEmpty String
explainHookupError e =
  case e of
    ConnectionFailure exs ->
      "Connect failed" :| map explainIOError exs

    LineTooLong ->
      "IRC message too long" :| []

    LineTruncated ->
      "IRC message incomplete" :| []

    -- the remaining messages can be used as is and on one line
    _ -> displayException e :| []
