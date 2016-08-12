{-|
Module      : Client.CApi
Description : Dynamically loaded extension API
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Client.CApi
  ( ActiveExtension
  , activateExtension
  , deactivateExtension
  , notifyExtensions
  ) where

import           Client.CApi.Types
import           Control.Lens
import           Control.Monad.Trans.Cont
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable
import           Irc.RawIrcMsg
import           Irc.UserInfo
import           System.Posix.DynamicLinker

data ActiveExtension = ActiveExtension
  { aeFgn     :: FgnExtension
  , aeDL      :: DL
  , aeSession :: Ptr ()
  }

activateExtension :: FilePath -> IO ActiveExtension
activateExtension path =
  do dl <- dlopen path [RTLD_LAZY, RTLD_LOCAL]
     p  <- castFunPtrToPtr <$> dlsym dl "extension"
     md <- peek p
     s  <- runStartExtension (fgnStart md)
     return ActiveExtension
       { aeFgn = md
       , aeDL  = dl
       , aeSession = s
       }

deactivateExtension :: ActiveExtension -> IO ()
deactivateExtension ae =
  do runStopExtension (fgnStop (aeFgn ae)) (aeSession ae)
     dlclose (aeDL ae)

notifyExtensions :: Text -> RawIrcMsg -> [ActiveExtension] -> IO ()
notifyExtensions _       _   []  = return ()
notifyExtensions network msg aes =
  withRawIrcMsg network msg $ \p ->
    for_ aes $ \ae ->
      runProcessMessage (fgnProcess (aeFgn ae)) (aeSession ae) p

withRawIrcMsg ::
  Text      {- ^ network -} ->
  RawIrcMsg {- ^ message -} ->
  (Ptr FgnMsg -> IO a) {- ^ continuation -} ->
  IO a
withRawIrcMsg network msg = runContT $
  do net     <- ContT $ withText network
     pfx     <- ContT $ withText $ maybe Text.empty renderUserInfo $ view msgPrefix msg
     cmd     <- ContT $ withText $ view msgCommand msg
     prms    <- traverse (ContT . withText) (view msgParams msg)
     (n,prm) <- ContT $ withArrayLen prms . curry
     ContT $ with $ FgnMsg net pfx cmd prm $ fromIntegral n

withText :: Text -> (FgnStringLen -> IO a) -> IO a
withText txt k =
  Text.withCStringLen txt $ \(ptr, len) ->
  k (FgnStringLen ptr (fromIntegral len))
