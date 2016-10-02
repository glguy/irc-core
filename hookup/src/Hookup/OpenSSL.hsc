{-# Language RankNTypes, TemplateHaskell #-}
{-|
Module      : Hookup.OpenSSL
Description : Hack into the internals of OpenSSL to add missing functionality
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com
-}

#include "openssl/ssl.h"
#include "openssl/x509_vfy.h"
#include "openssl/x509v3.h"

module Hookup.OpenSSL where

import Control.Concurrent.MVar
import Control.Monad
import Foreign.C
import Foreign.Ptr
import Language.Haskell.TH
import OpenSSL.Session

------------------------------------------------------------------------
-- Unfortunately hidden definitions from HsOpenSSL
------------------------------------------------------------------------

failIf_ :: (a -> Bool) -> a -> IO ()
failIf_ f a = when (f a) raiseOpenSSLError

raiseOpenSSLError :: IO a
raiseOpenSSLError = fail =<< errorString =<< err_get_error

foreign import ccall unsafe "ERR_get_error"
    err_get_error :: IO CULong

foreign import ccall unsafe "ERR_error_string"
    err_error_string :: CULong -> CString -> IO CString

errorString :: CULong -> IO String
errorString code = peekCString =<< err_error_string code nullPtr

------------------------------------------------------------------------
-- HsOpenSSL doesn't provide access to the underlying OpenSSL context
------------------------------------------------------------------------

withContext :: SSLContext -> (forall a. Ptr a -> IO b) -> IO b
withContext ctx f =
  $(do info <- reify ''SSLContext
       m    <- newName "m"
       case info of
         TyConI (DataD _ _ _ _ [RecC cn [_,_]] _) ->
           [| let $(conP cn [varP m,wildP]) = ctx in withMVar $(varE m) f |]
         _ -> fail "hookup: PANIC: SSLContext not matched"
   )

------------------------------------------------------------------------
-- Bindings to hostname verification interface
------------------------------------------------------------------------

data SSLParam_

-- X509_VERIFY_PARAM *SSL_CTX_get0_param(SSL_CTX *ctx);
foreign import ccall unsafe "SSL_CTX_get0_param"
  sslGet0Param :: Ptr a -> IO (Ptr SSLParam_)

-- void X509_VERIFY_PARAM_set_hostflags(X509_VERIFY_PARAM *param, unsigned int flags);
foreign import ccall unsafe "X509_VERIFY_PARAM_set_hostflags"
  x509VerifyParamSetHostflags :: Ptr SSLParam_ -> CUInt -> IO ()

-- int X509_VERIFY_PARAM_set1_host(X509_VERIFY_PARAM *param, const char *name, size_t namelen);
foreign import ccall unsafe "X509_VERIFY_PARAM_set1_host"
  x509VerifyParamSet1Host :: Ptr SSLParam_ -> CString -> CSize -> IO CInt

installVerification :: SSLContext -> String {- ^ hostname -} -> IO ()
installVerification ctx host =
  withContext ctx $ \ctxPtr ->
    do param <- sslGet0Param ctxPtr
       x509VerifyParamSetHostflags param
         (#const X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS)
       withCStringLen host $ \(ptr,len) ->
         x509VerifyParamSet1Host param ptr (fromIntegral len)
         >>= failIf_ (/= 1)
