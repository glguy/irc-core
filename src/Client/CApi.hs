{-# Language GeneralizedNewtypeDeriving, RankNTypes, RecordWildCards #-}
{-|
Module      : Client.CApi
Description : Dynamically loaded extension API
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

Foreign interface to the IRC client via a simple C API
and dynamically loaded modules.

-}

module Client.CApi
  ( -- * Extension type
    ActiveExtension(..)

  -- * Extension callbacks
  , extensionSymbol
  , openExtension
  , startExtension
  , stopExtension
  , notifyExtension
  , commandExtension
  , chatExtension

  , ThreadEntry(..)
  , threadFinish

  , popTimer
  , pushTimer
  , cancelTimer

  , evalNestedIO
  , withChat
  , withRawIrcMsg
  ) where

import Client.CApi.Types
import Client.Configuration (ExtensionConfiguration, extensionPath, extensionRtldFlags, extensionArgs)
import Control.Lens (view)
import Control.Monad (guard, unless)
import Control.Monad.Codensity (Codensity(Codensity), lowerCodensity)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IntPSQ (IntPSQ)
import Data.IntPSQ qualified as IntPSQ
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Foreign.C (peekCString, withCString)
import Foreign.Marshal (withArray, withArrayLen, with)
import Foreign.Ptr (Ptr, FunPtr, castFunPtrToPtr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable(peek))
import Irc.Identifier (idText)
import Irc.RawIrcMsg (RawIrcMsg(..), TagEntry(..))
import Irc.UserInfo (UserInfo(userHost, userNick, userName))
import System.Posix.DynamicLinker (dlopen, dlclose, dlsym, DL)

------------------------------------------------------------------------

-- | The symbol that is loaded from an extension object.
--
-- Extensions are expected to export:
--
-- @
-- struct galua_extension extension;
-- @
extensionSymbol :: String
extensionSymbol = "extension"

-- | Information about a loaded extension including the handle
-- to the loaded shared object, and state value returned by
-- the startup callback, and the loaded extension record.
data ActiveExtension = ActiveExtension
  { aeFgn     :: !FgnExtension -- ^ Struct of callback function pointers
  , aeDL      :: !DL           -- ^ Handle of dynamically linked extension
  , aeSession :: !(Ptr ())       -- ^ State value generated by start callback
  , aeName    :: !Text
  , aeMajorVersion, aeMinorVersion :: !Int
  , aeTimers  :: !(IntPSQ UTCTime TimerEntry)
  , aeNextTimer :: !Int
  , aeThreads :: !Int
  , aeLive    :: !Bool
  }

data TimerEntry = TimerEntry !(FunPtr TimerCallback) !(Ptr ())

data ThreadEntry = ThreadEntry !(FunPtr ThreadFinish) !(Ptr ())

-- | Find the earliest timer ready to run if any are available.
popTimer ::
  ActiveExtension {- ^ extension -} ->
  Maybe (UTCTime, TimerId, FunPtr TimerCallback, Ptr (), ActiveExtension)
    {- ^ earlier time, callback, callback state, updated extension -}
popTimer ae =
  do let timers = aeTimers ae
     guard (aeLive ae)
     (timerId, time, TimerEntry fun ptr, timers') <- IntPSQ.minView timers
     let ae' = ae { aeTimers = timers' }
     return (time, fromIntegral timerId, fun, ptr, ae')

-- | Schedue a new timer event for the given extension.
pushTimer ::
  UTCTime              {- ^ activation time   -} ->
  FunPtr TimerCallback {- ^ callback function -} ->
  Ptr ()               {- ^ callback state    -} ->
  ActiveExtension      {- ^ extension         -} ->
  (Int,ActiveExtension)
pushTimer time fun ptr ae = entry `seq` ae' `seq` (i, ae')
  where
    entry = TimerEntry fun ptr
    i     = aeNextTimer ae
    ae'   = ae { aeTimers = IntPSQ.insert i time entry (aeTimers ae)
               , aeNextTimer = i + 1 }

-- | Remove a timer from the schedule by ID
cancelTimer ::
  Int             {- ^ timer ID  -}  ->
  ActiveExtension {- ^ extension -}  ->
  Maybe (Ptr (), ActiveExtension)
cancelTimer timerId ae =
  do (_, TimerEntry _ ptr) <- IntPSQ.lookup timerId (aeTimers ae)
     return (ptr, ae { aeTimers = IntPSQ.delete timerId (aeTimers ae)})

-- | Load the extension from the given path and call the start
-- callback. The result of the start callback is saved to be
-- passed to any subsequent calls into the extension.
openExtension ::
  ExtensionConfiguration {- ^ extension configuration -} ->
  IO ActiveExtension
openExtension config =
  do dl   <- dlopen (view extensionPath config)
                    (view extensionRtldFlags config)
     p    <- dlsym dl extensionSymbol
     fgn  <- peek (castFunPtrToPtr p)
     name <- peekCString (fgnName fgn)
     return $! ActiveExtension
       { aeFgn          = fgn
       , aeDL           = dl
       , aeSession      = nullPtr
       , aeName         = Text.pack name
       , aeTimers       = IntPSQ.empty
       , aeMajorVersion = fromIntegral (fgnMajorVersion fgn)
       , aeMinorVersion = fromIntegral (fgnMinorVersion fgn)
       , aeNextTimer    = 1
       , aeThreads      = 0
       , aeLive         = True
       }

startExtension ::
  Ptr ()                 {- ^ client stable pointer   -} ->
  ExtensionConfiguration {- ^ extension configuration -} ->
  ActiveExtension        {- ^ active extension        -} ->
  IO (Ptr ())            {- ^ extension state         -}
startExtension stab config ae =
  do let f = fgnStart (aeFgn ae)
     if nullFunPtr == f
       then return nullPtr
       else evalNestedIO $
                  do extPath <- nest1 (withCString (view extensionPath config))
                     args <- traverse withText
                           $ view extensionArgs config
                     argsArray <- nest1 (withArray args)
                     let len = fromIntegral (length args)
                     liftIO (runStartExtension f stab extPath argsArray len)

stopExtension :: ActiveExtension -> IO ()
stopExtension ae =
  do let f = fgnStop (aeFgn ae)
     unless (nullFunPtr == f) $
       runStopExtension f (aeSession ae)
     dlclose (aeDL ae)

-- | Call all of the process chat callbacks in the list of extensions.
-- This operation marshals the IRC message once and shares that across
-- all of the callbacks.
--
-- Returns 'True' to pass message to client.  Returns 'False to drop message.
chatExtension ::
  ActiveExtension {- ^ extension               -} ->
  Ptr FgnChat     {- ^ serialized chat message -} ->
  IO Bool         {- ^ allow message           -}
chatExtension ae chat =
  do let f = fgnChat (aeFgn ae)
     if f == nullFunPtr
       then return True
       else (passMessage ==) <$> runProcessChat f (aeSession ae) chat

-- | Call all of the process message callbacks in the list of extensions.
-- This operation marshals the IRC message once and shares that across
-- all of the callbacks.
--
-- Returns 'True' to pass message to client.  Returns 'False to drop message.
notifyExtension ::
  ActiveExtension {- ^ extension                  -} ->
  Ptr FgnMsg      {- ^ serialized IRC message     -} ->
  IO Bool         {- ^ allow message              -}
notifyExtension ae msg =
  do let f = fgnMessage (aeFgn ae)
     if f == nullFunPtr
       then return True
       else (passMessage ==) <$> runProcessMessage f (aeSession ae) msg


-- | Notify an extension of a client command with the given parameters.
commandExtension ::
  Text            {- ^ command                -} ->
  ActiveExtension {- ^ extension to command   -} ->
  IO ()
commandExtension command ae = evalNestedIO $
  do cmd <- withCommand command
     let f = fgnCommand (aeFgn ae)
     liftIO $ unless (f == nullFunPtr)
            $ runProcessCommand f (aeSession ae) cmd

-- | Notify an extension that one of its threads has finished.
threadFinish :: ThreadEntry -> IO ()
threadFinish (ThreadEntry f x) = runThreadFinish f x

-- | Marshal a 'RawIrcMsg' into a 'FgnMsg' which will be valid for
-- the remainder of the computation.
withRawIrcMsg ::
  Text                 {- ^ network      -} ->
  RawIrcMsg            {- ^ message      -} ->
  NestedIO (Ptr FgnMsg)
withRawIrcMsg network RawIrcMsg{..} =
  do net     <- withText network
     pfxN    <- withText $ maybe Text.empty (idText.userNick) _msgPrefix
     pfxU    <- withText $ maybe Text.empty userName _msgPrefix
     pfxH    <- withText $ maybe Text.empty userHost _msgPrefix
     cmd     <- withText _msgCommand
     prms    <- traverse withText _msgParams
     tags    <- traverse withTag  _msgTags
     let (keys,vals) = unzip tags
     (tagN,keysPtr) <- nest2 $ withArrayLen keys
     valsPtr        <- nest1 $ withArray vals
     (prmN,prmPtr)  <- nest2 $ withArrayLen prms
     nest1 $ with $ FgnMsg net pfxN pfxU pfxH cmd prmPtr (fromIntegral prmN)
                                       keysPtr valsPtr (fromIntegral tagN)

withChat ::
  Text {- ^ network -} ->
  Text {- ^ target  -} ->
  Text {- ^ message -} ->
  NestedIO (Ptr FgnChat)
withChat net tgt msg =
  do net' <- withText net
     tgt' <- withText tgt
     msg' <- withText msg
     nest1 $ with $ FgnChat net' tgt' msg'

withCommand ::
  Text {- ^ command -} ->
  NestedIO (Ptr FgnCmd)
withCommand command =
  do cmd <- withText command
     nest1 $ with $ FgnCmd cmd

withTag :: TagEntry -> NestedIO (FgnStringLen, FgnStringLen)
withTag (TagEntry k v) =
  do pk <- withText k
     pv <- withText v
     return (pk,pv)

withText :: Text -> NestedIO FgnStringLen
withText txt =
  do (ptr,len) <- nest1 $ withText0 txt
     return $ FgnStringLen ptr $ fromIntegral len

------------------------------------------------------------------------

-- | Continuation-passing style bracked IO actions.
newtype NestedIO a = NestedIO (Codensity IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Return the bracket IO action.
evalNestedIO :: NestedIO a -> IO a
evalNestedIO (NestedIO m) = lowerCodensity m

-- | Wrap up a bracketing IO operation where the continuation takes 1 argument
nest1 :: (forall r. (a -> IO r) -> IO r) -> NestedIO a
nest1 f = NestedIO (Codensity f)

-- | Wrap up a bracketing IO operation where the continuation takes 2 argument
nest2 :: (forall r. (a -> b -> IO r) -> IO r) -> NestedIO (a,b)
nest2 f = NestedIO (Codensity (f . curry))
