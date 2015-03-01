-- | This module implements a simple rate limiter based on the
-- to be used to keep an IRC client from getting kicked due to
-- flooding. It allows one event per duration with a given threshold.
module Irc.RateLimit (RateLimit, newRateLimit, tickRateLimit) where

import Control.Concurrent
import Control.Monad
import Data.Time

data RateLimit = RateLimit
  { rateDebt :: MVar [UTCTime]
  , rateThreshold :: Int
  , rateDuration  :: Int
  }

newRateLimit :: Int -> Int -> IO RateLimit
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

tickRateLimit :: RateLimit -> IO ()
tickRateLimit r = modifyMVar_ (rateDebt r) $ \debt ->
  do now <- getCurrentTime
     let isActive t = truncate (diffUTCTime now t) < rateDuration r
         debt' = filter isActive debt

     if length debt' < rateThreshold r
       then return (now:debt')

       else do let wait = ceiling
                        $ 1000000 -- milliseconds
                        * ( fromIntegral (rateDuration r)
                          - realToFrac (diffUTCTime now (last debt'))
                          )
               print wait
               threadDelay wait
               return (now:init debt')
