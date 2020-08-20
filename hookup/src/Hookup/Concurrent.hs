{-# Language BlockArguments, ScopedTypeVariables #-}
{-|
Module      : Hookup.Concurrent
Description : Concurrently run actions until one succeeds or all fail
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Hookup.Concurrent (concurrentAttempts) where

import Control.Concurrent (forkIO, throwTo)
import Control.Concurrent.Async (Async, AsyncCancelled(..), async, asyncThreadId, cancel, waitCatchSTM)
import Control.Concurrent.STM (STM, atomically, check, orElse, readTVar, registerDelay, retry)
import Control.Exception (SomeException, finally, mask_, onException)
import Control.Monad (join)
import Data.Foldable (for_)

concurrentAttempts ::
  Int {- ^ microsecond delay between attempts -} ->
  [IO a] {- ^ ordered list of attempts -} ->
  IO (Either [SomeException] a)
concurrentAttempts _ [] = pure (Left [])
concurrentAttempts delay actions =
  let st = St { threads = [],
                errors = [],
                work = actions,
                delay = delay,
                readySTM = retry }
  in mask_ (stall st)

data St a = St
  { threads :: [Async a]
  , errors  :: [SomeException]
  , work    :: [IO a]
  , delay   :: !Int
  , readySTM :: STM ()
  }

-- non-blocking cancelation of the remaining threads
cleanup :: [Async a] -> IO ()
cleanup xs = () <$ forkIO (for_ xs \x -> throwTo (asyncThreadId x) AsyncCancelled)

type Answer a = IO (Either [SomeException] a)

start :: IO a -> St a -> Answer a
start x st =
  do thread <- async x
     ready <- if null (work st) then pure retry else startTimer (delay st)
     stall st { threads = thread : threads st, readySTM = ready }

stall :: forall a. St a -> Answer a
stall st = if null (threads st) then nothingRunning else waitForEvent
  where
    nothingRunning =
      case work st of
        [] -> pure (Left (errors st))
        x:xs -> start x st{ work = xs }

    waitForEvent = join (atomically (finish [] (threads st))
                         `onException` cleanup (threads st))

    fresh :: STM (Answer a)
    fresh =
      case work st of
        [] -> retry
        x:xs -> start x st{work = xs} <$ readySTM st

    finish :: [Async a] -> [Async a] -> STM (Answer a)
    finish threads' [] = fresh
    finish threads' (t:ts) = finish1 (threads' ++ ts) t
                    `orElse` finish (t:threads') ts

    finish1 :: [Async a] -> Async a -> STM (Answer a)
    finish1 threads' t =
      do res <- waitCatchSTM t
         pure case res of
           Right s -> Right s <$ cleanup threads'
           Left e -> stall st { errors = e : errors st, threads = threads'}

startTimer :: Int -> IO (STM ())
startTimer n =
  do v <- registerDelay n
     pure (check =<< readTVar v)
