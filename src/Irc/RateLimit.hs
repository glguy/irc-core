-- | This module implements a simple rate limiter based on the
-- to be used to keep an IRC client from getting kicked due to
-- flooding. It allows one event per duration with a given threshold.
module Irc.RateLimit (RateLimit, newRateLimit, tickRateLimit) where

import Control.Concurrent
import Control.Monad
import Data.Time

-- | The 'RateLimit' keeps track of rate limit settings as well
-- as the current state of the limit.
data RateLimit = RateLimit
  { rateDebt :: MVar [UTCTime]
  , rateThreshold :: Int
  , rateDuration  :: Int
  }

-- | Construct a new rate limit with the given duration and threshold.
newRateLimit ::
  Int {- ^ duration  -} ->
  Int {- ^ threshold -} ->
  IO RateLimit
newRateLimit duration threshold =
  do ref <- newMVar []

     unless (duration > 0)
        (fail "newRateLimit: Duration too small")

     unless (threshold > 0)
        (fail "newRateLimit: Threshold too small")

     return RateLimit
        { rateDebt = ref
        , rateThreshold = threshold
        , rateDuration  = duration
        }

-- | Account for an event in the context of a 'RateLimit'. This command
-- will block and delay as required to satisfy the current rate. Once
-- it returns it is safe to proceed with the rate limited action.
tickRateLimit :: RateLimit -> IO ()
tickRateLimit r = modifyMVar_ (rateDebt r) $ \debt ->
  do now <- getCurrentTime
     let isActive t = truncate (diffUTCTime now t) < rateDuration r
         debt' = filter isActive debt

     if length debt' < rateThreshold r
       then return (now:debt')

       else do let wait = ceiling
                        $ (1000000 :: Double) -- milliseconds
                        * ( fromIntegral (rateDuration r)
                          - realToFrac (diffUTCTime now (last debt'))
                          )
               print wait
               threadDelay wait
               return (now:init debt')
