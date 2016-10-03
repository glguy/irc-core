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

module Hookup.OpenSSL (installVerification) where

import           Control.Concurrent.MVar (MVar, withMVar)
import           Control.Monad (unless)
import           Foreign.C (CString(..), CSize(..), CUInt(..), CInt(..), withCStringLen)
import           Foreign.Ptr (Ptr)
import           Language.Haskell.TH (Info(..), Dec(..), Type(..), Con(..), varE, reify)
import           OpenSSL.Session (SSLContext)

------------------------------------------------------------------------
-- HsOpenSSL doesn't provide access to the underlying OpenSSL context
------------------------------------------------------------------------

do TyConI (DataD _ _ _ _ [RecC cn [(field,_,AppT _ (AppT _ sslcontext_)),_]] _) <- reify ''SSLContext

   [d| type SSLContext_ = $(pure sslcontext_)

       sslCtx :: SSLContext -> MVar (Ptr SSLContext_)
       sslCtx = $(varE field)
     |]

-- | Execute the action returned from applying a continuation to the SSL_CONTEXT pointer
-- contained within the 'SSLContext'. The 'SSLContext' will be locked during this
-- operation.
withContext :: SSLContext -> (Ptr SSLContext_ -> IO a) -> IO a
withContext = withMVar . sslCtx

------------------------------------------------------------------------
-- Bindings to hostname verification interface
------------------------------------------------------------------------

data X509_VERIFY_PARAM_

-- X509_VERIFY_PARAM *SSL_CTX_get0_param(SSL_CTX *ctx);
foreign import ccall unsafe "SSL_CTX_get0_param"
  sslGet0Param ::
    Ptr SSLContext_ {- ^ ctx -} ->
    IO (Ptr X509_VERIFY_PARAM_)

-- void X509_VERIFY_PARAM_set_hostflags(X509_VERIFY_PARAM *param, unsigned int flags);
foreign import ccall unsafe "X509_VERIFY_PARAM_set_hostflags"
  x509VerifyParamSetHostflags ::
    Ptr X509_VERIFY_PARAM_ {- ^ param -} ->
    CUInt                  {- ^ flags -} ->
    IO ()

-- int X509_VERIFY_PARAM_set1_host(X509_VERIFY_PARAM *param, const char *name, size_t namelen);
foreign import ccall unsafe "X509_VERIFY_PARAM_set1_host"
  x509VerifyParamSet1Host ::
    Ptr X509_VERIFY_PARAM_ {- ^ param                -} ->
    CString                {- ^ name                 -} ->
    CSize                  {- ^ namelen              -} ->
    IO CInt                {- ^ 1 success, 0 failure -}

-- | Add hostname checking to the certificate verification step.
-- Partial wildcards matching is disabled.
installVerification :: SSLContext -> String {- ^ hostname -} -> IO ()
installVerification ctx host =
  withContext ctx     $ \ctxPtr ->
  withCStringLen host $ \(ptr,len) ->
    do param <- sslGet0Param ctxPtr
       x509VerifyParamSetHostflags param
         (#const X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS)
       success <- x509VerifyParamSet1Host param ptr (fromIntegral len)
       unless (success == 1) (fail "Unable to set verification host")
