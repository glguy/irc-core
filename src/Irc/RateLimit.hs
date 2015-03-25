-- | This module implements a simple rate limiter based on the
-- to be used to keep an IRC client from getting kicked due to
-- flooding. It allows one event per duration with a given threshold.
module Irc.RateLimit
  ( RateLimit
  , newRateLimit
  , newRateLimitDefault
  , tickRateLimit
  ) where

import Control.Concurrent
import Control.Monad
import Data.Time

-- | The 'RateLimit' keeps track of rate limit settings as well
-- as the current state of the limit.
data RateLimit = RateLimit
  { rateStamp     :: !(MVar UTCTime)
  , rateThreshold :: !Int
  , ratePenalty   :: !Int
  }

-- | Construct a new rate limit with the RFC 2813 specified
-- 2 second penalty and 10 second threshold
newRateLimitDefault :: IO RateLimit
newRateLimitDefault = newRateLimit 2 10

-- | Construct a new rate limit with the given penalty and threshold.
newRateLimit ::
  Int {- ^ penalty  -} ->
  Int {- ^ threshold -} ->
  IO RateLimit
newRateLimit penalty threshold =
  do unless (penalty > 0)
        (fail "newRateLimit: Penalty too small")

     unless (threshold > 0)
        (fail "newRateLimit: Threshold too small")

     now <- getCurrentTime
     ref <- newMVar now

     return RateLimit
        { rateStamp     = ref
        , rateThreshold = threshold
        , ratePenalty   = penalty
        }

-- | Account for an event in the context of a 'RateLimit'. This command
-- will block and delay as required to satisfy the current rate. Once
-- it returns it is safe to proceed with the rate limited action.
tickRateLimit :: RateLimit -> IO ()
tickRateLimit r = modifyMVar_ (rateStamp r) $ \stamp ->
  do now <- getCurrentTime
     let stamp' = fromIntegral (ratePenalty r) `addUTCTime` max stamp now
         diff   = diffUTCTime stamp' now
         excess = diff - fromIntegral (rateThreshold r)

     when (excess > 0) (threadDelay (ceiling (1000000 * realToFrac excess)))

     return stamp'
