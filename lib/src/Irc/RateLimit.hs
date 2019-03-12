{-|
Module      : Irc.RateLimit
Description : Rate limit operations for IRC
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module implements a simple rate limiter based on the IRC RFC
to be used to keep an IRC client from getting disconnected for
flooding. It allows one event per duration with a given threshold.

This algorithm keeps track of the time at which the client may
start sending messages. Each message sent advances that time into
the future by the @penalty@. The client is allowed to transmit
up to @threshold@ seconds ahead of this time.

-}
module Irc.RateLimit
  ( RateLimit
  , newRateLimit
  , tickRateLimit
  ) where

import Control.Concurrent
import Control.Monad
import Data.Time

-- | The 'RateLimit' keeps track of rate limit settings as well
-- as the current state of the limit.
data RateLimit = RateLimit
  { rateStamp     :: !(MVar UTCTime) -- ^ Time that client can send
  , rateThreshold :: !NominalDiffTime
  , ratePenalty   :: !NominalDiffTime
  }

-- | Construct a new rate limit with the given penalty and threshold.
newRateLimit ::
  Rational {- ^ penalty seconds -} ->
  Rational {- ^ threshold seconds -} ->
  IO RateLimit
newRateLimit penalty threshold =
  do now <- getCurrentTime
     ref <- newMVar now

     return RateLimit
        { rateStamp     = ref
        , rateThreshold = realToFrac (max 0 threshold)
        , ratePenalty   = realToFrac (max 0 penalty)
        }

-- | Account for an event in the context of a 'RateLimit'. This command
-- will block and delay as required to satisfy the current rate. Once
-- it returns it is safe to proceed with the rate limited action.
tickRateLimit :: RateLimit -> IO ()
tickRateLimit r = modifyMVar_ (rateStamp r) $ \stamp ->
  do now <- getCurrentTime
     let stamp' = ratePenalty r `addUTCTime` max stamp now
         diff   = diffUTCTime stamp' now
         excess = diff - rateThreshold r

     when (excess > 0) (threadDelay (ceiling (1000000 * excess)))

     return stamp'
