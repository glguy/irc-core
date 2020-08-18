{-# Language BlockArguments, ScopedTypeVariables #-}
{-|
Module      : Hookup.Concurrent
Description : Concurrently run actions until one succeeds or all fail
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Hookup.Concurrent (concurrentAttempts) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async, async, cancel, waitCatchSTM)
import Control.Concurrent.STM (STM, atomically, check, orElse, readTVar, registerDelay, retry)
import Control.Exception (SomeException, finally, mask_, onException)
import Data.Foldable (traverse_)

concurrentAttempts ::
  Int {- ^ microsecond delay between attempts -} ->
  [IO a] {- ^ ordered list of attempts -} ->
  IO (Either [SomeException] a)
concurrentAttempts delay actions = mask_ (start (initialSt delay actions))

data St a = St
  { threads :: [Async a]
  , errors  :: [SomeException]
  , work    :: [IO a]
  , delay   :: !Int
  }

initialSt :: Int -> [IO a] -> St a
initialSt delay xs = St
  { threads = []
  , errors  = []
  , work    = xs
  , delay   = delay
  }

cleanup :: [Async a] -> IO ()
cleanup xs = () <$ forkIO (traverse_ cancel xs)

type Answer a = IO (Either [SomeException] a)

start :: St a -> Answer a
start st =
  case work st of
    [] -> stall st
    x:xs -> do thread <- async x
               stall st { threads = thread : threads st, work = xs }

stall :: forall a. St a -> Answer a
stall st
  | null (threads st), null (work st) = pure (Left (errors st))
  | otherwise =
  do fresh <- mkFresh
     next <- atomically (fresh `orElse` finish [] (threads st))
             `onException` cleanup (threads st)
     next
  where
    mkFresh :: IO (STM (Answer a))
    mkFresh
      | null (work st) = pure retry
      | otherwise  = do tv <- registerDelay (delay st)
                        pure do check =<< readTVar tv
                                pure (start st)

    finish :: [Async a] -> [Async a] -> STM (Answer a)
    finish threads' [] = retry
    finish threads' (t:ts) = finish1 (threads' ++ ts) t `orElse` finish (t:threads') ts

    finish1 :: [Async a] -> Async a -> STM (Answer a)
    finish1 threads' t =
      do res <- waitCatchSTM t
         pure case res of
           Right s -> Right s <$ cleanup threads'
           Left e -> start st { errors = e : errors st, threads = threads'}
