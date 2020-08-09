{-# Language CApiFFI #-}
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

#ifndef X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS
#error "OpenSSL 1.0.2 or later is required. This version was released in Jan 2015 and adds hostname verification"
#endif

module Hookup.OpenSSL (withDefaultPassword, installVerification, getPubKeyDer) where

import           Control.Exception (bracket, bracket_)
import           Control.Monad (unless)
import           Foreign.C (CStringLen, CString(..), CSize(..), CUInt(..), CInt(..), withCStringLen, CChar(..))
import           Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr, nullFunPtr)
import           Foreign.StablePtr (StablePtr, deRefStablePtr, castPtrToStablePtr)
import           Foreign.Marshal (with)
import           OpenSSL.Session (SSLContext, SSLContext_, withContext)
import           OpenSSL.X509 (withX509Ptr, X509, X509_)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as Unsafe

------------------------------------------------------------------------
-- Bindings to password callback
------------------------------------------------------------------------

foreign import ccall unsafe "hookup_new_userdata"
  hookup_new_userdata :: CString -> CInt -> IO (Ptr ())

foreign import ccall unsafe "hookup_free_userdata"
  hookup_free_userdata :: Ptr () -> IO ()

foreign import ccall "&hookup_pem_passwd_cb"
  hookup_pem_passwd_cb :: FunPtr PemPasswdCb

-- int pem_passwd_cb(char *buf, int size, int rwflag, void *userdata);
type PemPasswdCb = Ptr CChar -> CInt -> CInt -> Ptr () -> IO CInt

-- void SSL_CTX_set_default_passwd_cb(SSL_CTX *ctx, pem_password_cb *cb);
foreign import ccall unsafe "SSL_CTX_set_default_passwd_cb"
  sslCtxSetDefaultPasswdCb :: Ptr SSLContext_ -> FunPtr PemPasswdCb -> IO ()

-- void SSL_CTX_set_default_passwd_cb_userdata(SSL_CTX *ctx, void *u);
foreign import ccall unsafe "SSL_CTX_set_default_passwd_cb_userdata"
  sslCtxSetDefaultPasswdCbUserdata ::
    Ptr SSLContext_ -> Ptr a -> IO ()

withDefaultPassword :: SSLContext -> Maybe ByteString -> IO a -> IO a
withDefaultPassword ctx mbBs m =
  withCPassword mbBs $ \ptr len ->
  bracket (hookup_new_userdata ptr len) hookup_free_userdata $ \ud ->
  bracket_ (setup hookup_pem_passwd_cb ud) (setup nullFunPtr nullPtr) m

  where
  withCPassword Nothing k = k nullPtr (-1)
  withCPassword (Just bs) k = Unsafe.unsafeUseAsCStringLen bs $ \(ptr, len) -> k ptr (fromIntegral len)

  setup cb ud =
    withContext ctx $ \ctxPtr ->
    do sslCtxSetDefaultPasswdCb         ctxPtr cb
       sslCtxSetDefaultPasswdCbUserdata ctxPtr ud

------------------------------------------------------------------------
-- Bindings to hostname verification interface
------------------------------------------------------------------------

data X509_VERIFY_PARAM_
data {-# CTYPE "openssl/ssl.h" "X509_PUBKEY" #-} X509_PUBKEY_
data {-# CTYPE "openssl/ssl.h" "X509" #-} X509__

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

-- X509_PUBKEY *X509_get_X509_PUBKEY(X509 *x);
foreign import capi unsafe "openssl/x509.h X509_get_X509_PUBKEY"
  x509getX509Pubkey ::
    Ptr X509__ -> IO (Ptr X509_PUBKEY_)

-- int i2d_X509_PUBKEY(X509_PUBKEY *p, unsigned char **ppout);
foreign import ccall unsafe "i2d_X509_PUBKEY"
  i2dX509Pubkey ::
    Ptr X509_PUBKEY_ ->
    Ptr CString ->
    IO CInt

getPubKeyDer :: X509 -> IO ByteString
getPubKeyDer x509 =
  withX509Ptr x509 $ \x509ptr ->
  do pubkey <- x509getX509Pubkey (castPtr x509ptr)
     len    <- fromIntegral <$> i2dX509Pubkey pubkey nullPtr
     B.create len $ \bsPtr ->
        with (castPtr bsPtr) $ \ptrPtr ->
           () <$ i2dX509Pubkey pubkey ptrPtr


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
