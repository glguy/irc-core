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
import Control.Concurrent.Async (Async, AsyncCancelled(..), async, asyncThreadId, cancel, waitCatch, waitCatchSTM)
import Control.Concurrent.STM (STM, atomically, check, orElse, readTVar, registerDelay, retry)
import Control.Exception (SomeException, finally, mask_, onException)
import Control.Monad (join)
import Data.Foldable (for_)

concurrentAttempts ::
  Int {- ^ microsecond delay between attempts -} ->
  (a -> IO ()) {- ^ release unneeded success -} ->
  [IO a] {- ^ ordered list of attempts -} ->
  IO (Either [SomeException] a)
concurrentAttempts delay release actions =
  let st = St { threads = [],
                errors = [],
                work = actions,
                delay = delay,
                clean = release,
                readySTM = retry }
  in mask_ (loop st)

data St a = St
  { threads :: [Async a]
  , errors  :: [SomeException]
  , work    :: [IO a]
  , delay   :: !Int
  , clean   :: a -> IO ()
  , readySTM :: STM ()
  }

type Answer a = IO (Either [SomeException] a)

-- | Main event loop for concurrent attempt system
loop :: forall a. St a -> Answer a
loop st = if null (threads st) then nothingRunning st else waitForEvent st

-- | No threads are active, either start a new thread or return the complete error list
nothingRunning :: St a -> Answer a
nothingRunning st =
  case work st of
    [] -> pure (Left (errors st))
    x:xs -> start x st{work = xs}

-- | Start a new thread for the given attempt
start :: IO a -> St a -> Answer a
start x st =
  do thread <- async x
     ready <- if null (work st) then pure retry else startTimer (delay st)
     loop st { threads = thread : threads st, readySTM = ready }

-- Nothing to do but wait for a thread to finish or the timer to fire
waitForEvent :: St a -> Answer a
waitForEvent st = join (atomically (finish st [] (threads st))
                  `onException` cleanup (clean st) (threads st))

-- Search for an event out of the active threads and timer
finish :: St a -> [Async a] -> [Async a] -> STM (Answer a)
finish st threads' [] = fresh st
finish st threads' (t:ts) = finish1 st (threads' ++ ts) t
                   `orElse` finish st (t:threads') ts

-- Handle a thread completion event
finish1 :: St a -> [Async a] -> Async a -> STM (Answer a)
finish1 st threads' t =
      do res <- waitCatchSTM t
         pure case res of
           Right s -> Right s <$ cleanup (clean st) threads'
           Left e -> loop st { errors = e : errors st, threads = threads'}

-- Handle a new thread timer event
fresh :: St a -> STM (Answer a)
fresh st =
  case work st of
    [] -> retry
    x:xs -> start x st{work = xs} <$ readySTM st

-- | Create an STM action that only succeeds after at least 'n' microseconds have passed.
startTimer :: Int -> IO (STM ())
startTimer n =
  do v <- registerDelay n
     pure (check =<< readTVar v)

-- non-blocking cancelation of the remaining threads
cleanup :: (a -> IO ()) -> [Async a] -> IO ()
cleanup release xs =
  () <$ forkIO do for_ xs \x -> throwTo (asyncThreadId x) AsyncCancelled
                  for_ xs \x -> do res <- waitCatch x
                                   for_ res release
