{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module OpenSSL.DH.Internal (
    DH_,
    DHP,
    withDHPPtr,
    wrapDHPPtrWith,
    wrapDHPPtr,
    DH,
    withDHPtr,
    wrapDHPtrWith,
    wrapDHPtr,
    asDH,
    asDHP
  ) where

import Control.Applicative ((<$>))
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import qualified Foreign.Concurrent as FC

data DH_
newtype DHP = DHP (ForeignPtr DH_)

withDHPPtr :: DHP -> (Ptr DH_ -> IO a) -> IO a
withDHPPtr (DHP fp) = withForeignPtr fp

wrapDHPPtrWith :: (Ptr DH_ -> IO ()) -> Ptr DH_ -> IO DHP
wrapDHPPtrWith fin p = DHP <$> FC.newForeignPtr p (fin p)

wrapDHPPtr :: Ptr DH_ -> IO DHP
wrapDHPPtr = wrapDHPPtrWith _DH_free

newtype DH = DH (ForeignPtr DH_)

withDHPtr :: DH -> (Ptr DH_ -> IO a) -> IO a
withDHPtr (DH fp) = withForeignPtr fp

wrapDHPtrWith :: (Ptr DH_ -> IO ()) -> Ptr DH_ -> IO DH
wrapDHPtrWith fin p = DH <$> FC.newForeignPtr p (fin p)

wrapDHPtr :: Ptr DH_ -> IO DH
wrapDHPtr = wrapDHPtrWith _DH_free

asDH :: DHP -> DH
asDH (DHP fp) = DH fp

asDHP :: DH -> DHP
asDHP (DH fp) = DHP fp

foreign import ccall "DH_free"
  _DH_free :: Ptr DH_ -> IO ()

