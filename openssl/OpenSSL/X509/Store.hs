{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK prune             #-}
-- |An interface to X.509 certificate store.
module OpenSSL.X509.Store
    ( X509Store
    , X509_STORE -- private

    , newX509Store

    , wrapX509Store -- private
    , withX509StorePtr -- private

    , addCertToStore
    , addCRLToStore

    , X509StoreCtx
    , X509_STORE_CTX -- private

    , withX509StoreCtxPtr -- private
    , wrapX509StoreCtx -- private

    , getStoreCtxCert
    , getStoreCtxIssuer
    , getStoreCtxCRL
    , getStoreCtxChain
    )
    where
import Control.Applicative ((<$>))
import Control.Exception (throwIO, mask_)
import Foreign
import Foreign.C
import Foreign.Concurrent as FC
import OpenSSL.X509
import OpenSSL.X509.Revocation
import OpenSSL.Stack
import OpenSSL.Utils

-- |@'X509Store'@ is an opaque object that represents X.509
-- certificate store. The certificate store is usually used for chain
-- verification.
newtype X509Store  = X509Store (ForeignPtr X509_STORE)
data    X509_STORE


foreign import ccall unsafe "X509_STORE_new"
        _new :: IO (Ptr X509_STORE)

foreign import ccall unsafe "X509_STORE_free"
        _free :: Ptr X509_STORE -> IO ()

foreign import ccall unsafe "X509_STORE_add_cert"
        _add_cert :: Ptr X509_STORE -> Ptr X509_ -> IO CInt

foreign import ccall unsafe "X509_STORE_add_crl"
        _add_crl :: Ptr X509_STORE -> Ptr X509_CRL -> IO CInt

-- |@'newX509Store'@ creates an empty X.509 certificate store.
newX509Store :: IO X509Store
newX509Store = _new
               >>= failIfNull
               >>= \ ptr -> wrapX509Store (_free ptr) ptr

wrapX509Store :: IO () -> Ptr X509_STORE -> IO X509Store
wrapX509Store finaliser ptr
    = do fp <- newForeignPtr_ ptr
         FC.addForeignPtrFinalizer fp finaliser
         return $ X509Store fp

withX509StorePtr :: X509Store -> (Ptr X509_STORE -> IO a) -> IO a
withX509StorePtr (X509Store store)
    = withForeignPtr store

-- |@'addCertToStore' store cert@ adds a certificate to store.
addCertToStore :: X509Store -> X509 -> IO ()
addCertToStore store cert
    = withX509StorePtr store $ \ storePtr ->
      withX509Ptr cert       $ \ certPtr  ->
      _add_cert storePtr certPtr
           >>= failIf (/= 1)
           >>  return ()

-- |@'addCRLToStore' store crl@ adds a revocation list to store.
addCRLToStore :: X509Store -> CRL -> IO ()
addCRLToStore store crl
    = withX509StorePtr store $ \ storePtr ->
      withCRLPtr crl         $ \ crlPtr   ->
      _add_crl storePtr crlPtr
           >>= failIf (/= 1)
           >>  return ()

data    X509_STORE_CTX
newtype X509StoreCtx = X509StoreCtx (ForeignPtr X509_STORE_CTX)

foreign import ccall unsafe "X509_STORE_CTX_get_current_cert"
  _store_ctx_get_current_cert :: Ptr X509_STORE_CTX -> IO (Ptr X509_)

foreign import ccall unsafe "HsOpenSSL_X509_STORE_CTX_get0_current_issuer"
  _store_ctx_get0_current_issuer :: Ptr X509_STORE_CTX -> IO (Ptr X509_)

foreign import ccall unsafe "HsOpenSSL_X509_STORE_CTX_get0_current_crl"
  _store_ctx_get0_current_crl :: Ptr X509_STORE_CTX -> IO (Ptr X509_CRL)

foreign import ccall unsafe "X509_STORE_CTX_get_chain"
  _store_ctx_get_chain :: Ptr X509_STORE_CTX -> IO (Ptr STACK)

foreign import ccall unsafe "HsOpenSSL_X509_ref"
  _x509_ref :: Ptr X509_ -> IO ()

foreign import ccall unsafe "HsOpenSSL_X509_CRL_ref"
  _crl_ref :: Ptr X509_CRL -> IO ()

withX509StoreCtxPtr :: X509StoreCtx -> (Ptr X509_STORE_CTX -> IO a) -> IO a
withX509StoreCtxPtr (X509StoreCtx fp) = withForeignPtr fp

wrapX509StoreCtx :: IO () -> Ptr X509_STORE_CTX -> IO X509StoreCtx
wrapX509StoreCtx finaliser ptr =
  X509StoreCtx <$> FC.newForeignPtr ptr finaliser

getStoreCtxCert :: X509StoreCtx -> IO X509
getStoreCtxCert ctx = withX509StoreCtxPtr ctx $ \pCtx -> do
  pCert <- _store_ctx_get_current_cert pCtx
  if pCert == nullPtr
    then throwIO $ userError "BUG: NULL certificate in X509_STORE_CTX"
    else mask_ $ _x509_ref pCert >> wrapX509 pCert

getStoreCtxIssuer :: X509StoreCtx -> IO (Maybe X509)
getStoreCtxIssuer ctx = withX509StoreCtxPtr ctx $ \pCtx -> do
  pCert <- _store_ctx_get0_current_issuer pCtx
  if pCert == nullPtr
    then return Nothing
    else fmap Just $ mask_ $ _x509_ref pCert >> wrapX509 pCert

getStoreCtxCRL :: X509StoreCtx -> IO (Maybe CRL)
getStoreCtxCRL ctx = withX509StoreCtxPtr ctx $ \pCtx -> do
  pCrl <- _store_ctx_get0_current_crl pCtx
  if pCrl == nullPtr
    then return Nothing
    else fmap Just $ mask_ $ _crl_ref pCrl >> wrapCRL pCrl

getStoreCtxChain :: X509StoreCtx -> IO [X509]
getStoreCtxChain ctx = withX509StoreCtxPtr ctx $ \pCtx -> do
  stack <- _store_ctx_get_chain pCtx
  (`mapStack` stack) $ \pCert -> mask_ $ _x509_ref pCert >> wrapX509 pCert

