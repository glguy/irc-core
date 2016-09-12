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
import           Network.Connection
import           Network.TLS
import           Network.Socks5

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

  -- TLS package errors
  | Just tls <- fromException ex = explainTLSException tls

  -- connection package errors
  | Just (HostNotResolved str) <- fromException ex =
      ("Host not resolved: " ++ str) :| []

  | Just (HostCannotConnect str exs) <- fromException ex =
      ("Host cannot connect: " ++ str) :| map explainIOError exs

  | Just LineTooLong <- fromException ex = "Server IRC message too long" :| []

  -- socks package errors
  | Just err <- fromException ex = explainSocksError err :| []

  -- IOErrors, typically network package.
  | Just ioe <- fromException ex =
     explainIOError ioe :| []

  -- Anything else including glirc's errors (which use displayException)
  | otherwise = displayException ex :| []

explainIOError :: IOError -> String
explainIOError ioe = "IO error: " ++ displayException ioe

explainTLSException :: TLSException -> NonEmpty String
explainTLSException ex =
  case ex of
    ConnectionNotEstablished ->
      "Attempt to use connection out of order" :| []
    Terminated _ _ tlsError ->
        "Connection closed due to early-termination in TLS layer"
      :| explainTLSError tlsError
    HandshakeFailed (Error_Packet_Parsing str) ->
        "Connection closed due to handshake failure in TLS layer" :|
      [ "Packet parse error: " ++ str
      , "Please verify you're using a TLS enabled port"
      ]
    HandshakeFailed tlsError ->
        "Connection closed due to handshake failure in TLS layer"
      :| explainTLSError tlsError

explainTLSError :: TLSError -> [String]
explainTLSError ex =
  case ex of
    Error_Misc str                 -> ["Miscellaneous error: " ++ str]
    Error_Protocol (str, _, _desc) -> ["Protocol error: "      ++ str]
    Error_Certificate str          -> ["Certificate error: "   ++ str]
    Error_HandshakePolicy str      -> ["Handshake policy: "    ++ str]
    Error_EOF                      -> ["Unexpected end of connection"]
    Error_Packet str               -> ["Packet error: "        ++ str]
    Error_Packet_unexpected msg expect -> ("Packet unexpected: " ++ msg)
                                        : [ expect | not (null expect) ]
    Error_Packet_Parsing str       -> ["Packet parse error: " ++ str]

explainSocksError :: SocksError -> String
explainSocksError ex =
  case ex of
    SocksErrorGeneralServerFailure       -> "SOCKS: General server failure"
    SocksErrorConnectionNotAllowedByRule -> "SOCKS: Connection not allowed by rule"
    SocksErrorNetworkUnreachable         -> "SOCKS: Network unreachable"
    SocksErrorHostUnreachable            -> "SOCKS: Host unreachable"
    SocksErrorConnectionRefused          -> "SOCKS: Connection refused"
    SocksErrorTTLExpired                 -> "SOCKS: TTL Expired"
    SocksErrorCommandNotSupported        -> "SOCKS: Command not supported"
    SocksErrorAddrTypeNotSupported       -> "SOCKS: Address type not supported"
    SocksErrorOther n                    -> "SOCKS: Unknown error " ++ show n
