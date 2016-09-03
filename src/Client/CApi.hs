{-# Language RecordWildCards #-}
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
  , activateExtension
  , deactivateExtension
  , notifyExtensions
  , commandExtension
  ) where

import           Client.CApi.Types
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import           Foreign.C
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable
import           Irc.Identifier
import           Irc.RawIrcMsg
import           Irc.UserInfo
import           System.Posix.DynamicLinker

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
  }

-- | Load the extension from the given path and call the start
-- callback. The result of the start callback is saved to be
-- passed to any subsequent calls into the extension.
activateExtension ::
  Ptr () ->
  FilePath {- ^ path to extension -} ->
  IO ActiveExtension
activateExtension stab path =
  do dl   <- dlopen path [RTLD_NOW, RTLD_LOCAL]
     p    <- dlsym dl extensionSymbol
     fgn  <- peek (castFunPtrToPtr p)
     name <- peekCString (fgnName fgn)
     let f = fgnStart fgn
     s  <- if nullFunPtr == f
             then return nullPtr
             else withCString path (runStartExtension f stab)
     return $! ActiveExtension
       { aeFgn     = fgn
       , aeDL      = dl
       , aeSession = s
       , aeName    = Text.pack name
       , aeMajorVersion = fromIntegral (fgnMajorVersion fgn)
       , aeMinorVersion = fromIntegral (fgnMinorVersion fgn)
       }

-- | Call the stop callback of the extension if it is defined
-- and unload the shared object.
deactivateExtension :: Ptr () -> ActiveExtension -> IO ()
deactivateExtension stab ae =
  do let f = fgnStop (aeFgn ae)
     unless (nullFunPtr == f) $
       runStopExtension f stab (aeSession ae)
     dlclose (aeDL ae)

-- | Call all of the process message callbacks in the list of extensions.
-- This operation marshals the IRC message once and shares that across
-- all of the callbacks.
notifyExtensions ::
  Ptr ()            {- ^ clientstate stable pointer -} ->
  Text              {- ^ network              -} ->
  RawIrcMsg         {- ^ current message      -} ->
  [ActiveExtension] ->
  IO Bool {- ^ Return 'True' to pass message -}
notifyExtensions stab network msg aes
  | null aes' = return True
  | otherwise = doNotifications
  where
    aes' = [ (f,s) | ae <- aes
                  , let f = fgnMessage (aeFgn ae)
                        s = aeSession ae
                  , f /= nullFunPtr ]

    doNotifications =
      runContT (withRawIrcMsg network msg) (go aes')

    -- run handlers until one of them drops the message
    go [] _ = return True
    go ((f,s):rest) msgPtr =
       do res <- runProcessMessage f stab s msgPtr
          if res == passMessage
            then go rest msgPtr
            else return False

-- | Notify an extension of a client command with the given parameters.
commandExtension ::
  Ptr ()          {- ^ client state stableptr -} ->
  [Text]          {- ^ parameters             -} ->
  ActiveExtension {- ^ extension to command   -} ->
  IO ()
commandExtension stab params ae = evalContT $
  do cmd <- withCommand params
     let f = fgnCommand (aeFgn ae)
     lift $ unless (f == nullFunPtr)
          $ runProcessCommand f stab (aeSession ae) cmd

-- | Marshal a 'RawIrcMsg' into a 'FgnMsg' which will be valid for
-- the remainder of the computation.
withRawIrcMsg ::
  Text                 {- ^ network      -} ->
  RawIrcMsg            {- ^ message      -} ->
  ContT a IO (Ptr FgnMsg)
withRawIrcMsg network RawIrcMsg{..} =
  do net     <- withText network
     pfxN    <- withText $ maybe Text.empty (idText.userNick) _msgPrefix
     pfxU    <- withText $ maybe Text.empty userName _msgPrefix
     pfxH    <- withText $ maybe Text.empty userHost _msgPrefix
     cmd     <- withText _msgCommand
     prms    <- traverse withText _msgParams
     tags    <- traverse withTag  _msgTags
     let (keys,vals) = unzip tags
     (tagN,keysPtr) <- contT2 $ withArrayLen keys
     valsPtr        <- ContT  $ withArray vals
     (prmN,prmPtr)  <- contT2 $ withArrayLen prms
     ContT $ with $ FgnMsg net pfxN pfxU pfxH cmd prmPtr (fromIntegral prmN)
                                       keysPtr valsPtr (fromIntegral tagN)

withCommand ::
  [Text] {- ^ parameters -} ->
  ContT a IO (Ptr FgnCmd)
withCommand params =
  do prms          <- traverse withText params
     (prmN,prmPtr) <- contT2 $ withArrayLen prms
     ContT $ with $ FgnCmd prmPtr (fromIntegral prmN)

withTag :: TagEntry -> ContT a IO (FgnStringLen, FgnStringLen)
withTag (TagEntry k v) =
  do pk <- withText k
     pv <- withText v
     return (pk,pv)

withText :: Text -> ContT a IO FgnStringLen
withText txt =
  do (ptr,len) <- ContT $ Text.withCStringLen txt
     return $ FgnStringLen ptr $ fromIntegral len

contT2 :: ((a -> b -> m c) -> m c) -> ContT c m (a,b)
contT2 f = ContT $ \g -> f $ curry g
