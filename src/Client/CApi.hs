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
import           Control.Monad
import           Control.Monad.Trans.Class
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
     let f = fgnStart md
     s  <- if nullFunPtr == f
             then return nullPtr
             else runStartExtension f
     return ActiveExtension
       { aeFgn     = md
       , aeDL      = dl
       , aeSession = s
       }

deactivateExtension :: ActiveExtension -> IO ()
deactivateExtension ae =
  do let f = fgnStop (aeFgn ae)
     unless (nullFunPtr == f) (runStopExtension f (aeSession ae))
     dlclose (aeDL ae)

notifyExtensions :: Text -> RawIrcMsg -> [ActiveExtension] -> IO ()
notifyExtensions network msg aes = evalContT $
  do let getFun = fgnProcess . aeFgn
         aes' = filter (\ae -> getFun ae /= nullFunPtr) aes

     unitContT $ unless $ null aes'
     p  <- withRawIrcMsg network msg
     ae <- ContT $ for_ aes'
     lift (runProcessMessage (getFun ae) (aeSession ae) p)

withRawIrcMsg ::
  Text                 {- ^ network      -} ->
  RawIrcMsg            {- ^ message      -} ->
  ContT a IO (Ptr FgnMsg)
withRawIrcMsg network msg =
  do net     <- withText network
     pfx     <- withText $ maybe Text.empty renderUserInfo $ view msgPrefix msg
     cmd     <- withText $ view msgCommand msg
     prms    <- traverse withText $ view msgParams msg
     (n,prm) <- ContT $ withArrayLen prms . curry
     ContT $ with $ FgnMsg net pfx cmd prm $ fromIntegral n

withText :: Text -> ContT a IO FgnStringLen
withText txt =
  do (ptr,len) <- ContT $ Text.withCStringLen txt
     return $ FgnStringLen ptr $ fromIntegral len

unitContT :: (m a -> m a) -> ContT a m ()
unitContT f = ContT $ \g -> f (g ())
