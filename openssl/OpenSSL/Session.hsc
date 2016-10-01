{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE DeriveFoldable              #-}
{-# LANGUAGE DeriveTraversable           #-}
{-# LANGUAGE EmptyDataDecls              #-}
{-# LANGUAGE ExistentialQuantification   #-}
{-# LANGUAGE ForeignFunctionInterface    #-}
{-# LANGUAGE NamedFieldPuns              #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- | Functions for handling SSL connections. These functions use GHC specific
--   calls to cooperative the with the scheduler so that 'blocking' functions
--   only actually block the Haskell thread, not a whole OS thread.
module OpenSSL.Session
  ( -- * Contexts
    SSLContext
  , context
  , contextAddOption
  , contextRemoveOption
  , contextSetPrivateKey
  , contextSetCertificate
  , contextSetPrivateKeyFile
  , contextSetCertificateFile
  , contextSetCertificateChainFile
  , contextSetCiphers
  , contextSetDefaultCiphers
  , contextCheckPrivateKey
  , VerificationMode(..)
  , contextSetVerificationMode
  , contextSetCAFile
  , contextSetCADirectory
  , contextGetCAStore

    -- * SSL connections
  , SSL
  , SSLResult(..)
  , connection
  , fdConnection
  , addOption
  , removeOption
  , accept
  , tryAccept
  , connect
  , tryConnect
  , read
  , tryRead
  , readPtr
  , tryReadPtr
  , write
  , tryWrite
  , writePtr
  , tryWritePtr
  , lazyRead
  , lazyWrite
  , shutdown
  , tryShutdown
  , ShutdownType(..)
  , getPeerCertificate
  , getVerifyResult
  , sslSocket
  , sslFd

    -- * Protocol Options
  , SSLOption(..)

    -- * SSL Exceptions
  , SomeSSLException
  , ConnectionAbruptlyTerminated
  , ProtocolError(..)

    -- * Certificate hostname verification
  , installVerification
  ) where

#include "openssl/ssl.h"
#include "openssl/x509_vfy.h"
#include "openssl/x509v3.h"

import Prelude hiding (
#if !MIN_VERSION_base(4,6,0)
  catch,
#endif
  read, ioError, mapM, mapM_)
import Control.Concurrent (threadWaitWrite, threadWaitRead, runInBoundThread)
import Control.Concurrent.MVar
import Control.Exception
import Control.Applicative ((<$>), (<$))
import Control.Monad (unless)
import Data.Typeable
import Data.Foldable (Foldable, mapM_, forM_)
import Data.Traversable (Traversable, mapM)
import Data.Maybe (fromMaybe)
import Data.IORef
import Foreign
import Foreign.C
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import System.IO.Unsafe
import System.Posix.Types (Fd(..))
import Network.Socket (Socket(..))

import OpenSSL.ERR
import OpenSSL.EVP.PKey
import OpenSSL.EVP.Internal
import OpenSSL.SSL.Option
import OpenSSL.Utils
import OpenSSL.X509 (X509, X509_, wrapX509, withX509Ptr)
import OpenSSL.X509.Store

type VerifyCb = Bool -> Ptr X509_STORE_CTX -> IO Bool

foreign import ccall "wrapper" mkVerifyCb :: VerifyCb -> IO (FunPtr VerifyCb)

data SSLContext_
-- | An SSL context. Contexts carry configuration such as a server's private
--   key, root CA certiifcates etc. Contexts are stateful IO objects; they
--   start empty and various options are set on them by the functions in this
--   module. Note that an empty context will pretty much cause any operation to
--   fail since it doesn't even have any ciphers enabled.
data SSLContext = SSLContext { ctxMVar :: MVar (Ptr SSLContext_)
                             , ctxVfCb :: IORef (Maybe (FunPtr VerifyCb))
                             }
                deriving Typeable

data SSLMethod_

foreign import ccall unsafe "SSL_CTX_new" _ssl_ctx_new :: Ptr SSLMethod_ -> IO (Ptr SSLContext_)
foreign import ccall unsafe "SSL_CTX_free" _ssl_ctx_free :: Ptr SSLContext_ -> IO ()
foreign import ccall unsafe "SSLv23_method" _ssl_method :: IO (Ptr SSLMethod_)

-- | Create a new SSL context.
context :: IO SSLContext
context = mask_ $ do
  ctx   <- _ssl_method >>= _ssl_ctx_new
  cbRef <- newIORef Nothing
  mvar  <- newMVar ctx
#if MIN_VERSION_base(4,6,0)
  _     <- mkWeakMVar mvar
#else
  _     <- addMVarFinalizer mvar
#endif
           $ do _ssl_ctx_free ctx
                readIORef cbRef >>= mapM_ freeHaskellFunPtr
  return $ SSLContext { ctxMVar = mvar, ctxVfCb = cbRef }

-- | Run the given action with the raw context pointer and obtain the lock
--   while doing so.
withContext :: SSLContext -> (Ptr SSLContext_ -> IO a) -> IO a
withContext = withMVar . ctxMVar

touchContext :: SSLContext -> IO ()
touchContext = (>> return ()) . isEmptyMVar . ctxMVar

foreign import ccall unsafe "HsOpenSSL_SSL_CTX_set_options"
    _SSL_CTX_set_options :: Ptr SSLContext_ -> CLong -> IO CLong

foreign import ccall unsafe "HsOpenSSL_SSL_CTX_clear_options"
    _SSL_CTX_clear_options :: Ptr SSLContext_ -> CLong -> IO CLong

-- | Add a protocol option to the context.
contextAddOption :: SSLContext -> SSLOption -> IO ()
contextAddOption ctx opt =
    withContext ctx $ \ctxPtr ->
        _SSL_CTX_set_options ctxPtr (optionToIntegral opt) >> return ()

-- | Remove a protocol option from the context.
contextRemoveOption :: SSLContext -> SSLOption -> IO ()
contextRemoveOption ctx opt =
    withContext ctx $ \ctxPtr ->
        _SSL_CTX_clear_options ctxPtr (optionToIntegral opt) >> return ()

contextLoadFile :: (Ptr SSLContext_ -> CString -> CInt -> IO CInt)
                -> SSLContext -> String -> IO ()
contextLoadFile f context path =
  withContext context $ \ctx ->
    withCString path $ \cpath -> do
      result <- f ctx cpath (#const SSL_FILETYPE_PEM)
      unless (result == 1)
          $ f ctx cpath (#const SSL_FILETYPE_ASN1) >>= failIf_ (/= 1)

foreign import ccall unsafe "SSL_CTX_use_PrivateKey"
    _ssl_ctx_use_privatekey :: Ptr SSLContext_ -> Ptr EVP_PKEY -> IO CInt
foreign import ccall unsafe "SSL_CTX_use_certificate"
    _ssl_ctx_use_certificate :: Ptr SSLContext_ -> Ptr X509_ -> IO CInt

-- | Install a private key into a context.
contextSetPrivateKey :: KeyPair k => SSLContext -> k -> IO ()
contextSetPrivateKey context key
    = withContext context $ \ ctx    ->
      withPKeyPtr' key    $ \ keyPtr ->
          _ssl_ctx_use_privatekey ctx keyPtr
               >>= failIf_ (/= 1)

-- | Install a certificate (public key) into a context.
contextSetCertificate :: SSLContext -> X509 -> IO ()
contextSetCertificate context cert
    = withContext context $ \ ctx     ->
      withX509Ptr cert    $ \ certPtr ->
          _ssl_ctx_use_certificate ctx certPtr
               >>= failIf_ (/= 1)

foreign import ccall unsafe "SSL_CTX_use_PrivateKey_file"
   _ssl_ctx_use_privatekey_file :: Ptr SSLContext_ -> CString -> CInt -> IO CInt
foreign import ccall unsafe "SSL_CTX_use_certificate_file"
   _ssl_ctx_use_certificate_file :: Ptr SSLContext_ -> CString -> CInt -> IO CInt

-- | Install a private key file in a context. The key is given as a path to the
--   file which contains the key. The file is parsed first as PEM and, if that
--   fails, as ASN1. If both fail, an exception is raised.
contextSetPrivateKeyFile :: SSLContext -> FilePath -> IO ()
contextSetPrivateKeyFile = contextLoadFile _ssl_ctx_use_privatekey_file

-- | Install a certificate (public key) file in a context. The key is given as
--   a path to the file which contains the key. The file is parsed first as PEM
--   and, if that fails, as ASN1. If both fail, an exception is raised.
contextSetCertificateFile :: SSLContext -> FilePath -> IO ()
contextSetCertificateFile = contextLoadFile _ssl_ctx_use_certificate_file

foreign import ccall unsafe "SSL_CTX_use_certificate_chain_file"
   _ssl_ctx_use_certificate_chain_file :: Ptr SSLContext_ -> CString -> IO CInt

-- | Install a certificate chain in a context. The certificates must be in PEM
-- format and must be sorted starting with the subject's certificate (actual
-- client or server certificate), followed by intermediate CA certificates if
-- applicable, and ending at the highest level (root) CA.
contextSetCertificateChainFile :: SSLContext -> FilePath -> IO ()
contextSetCertificateChainFile context path =
  withContext context $ \ctx ->
    withCString path $ \cpath ->
      _ssl_ctx_use_certificate_chain_file ctx cpath >>= failIf_ (/= 1)

foreign import ccall unsafe "SSL_CTX_set_cipher_list"
   _ssl_ctx_set_cipher_list :: Ptr SSLContext_ -> CString -> IO CInt

-- | Set the ciphers to be used by the given context. The string argument is a
--   list of ciphers, comma separated, as given at
--   http://www.openssl.org/docs/apps/ciphers.html
--
--   Unrecognised ciphers are ignored. If no ciphers from the list are
--   recognised, an exception is raised.
contextSetCiphers :: SSLContext -> String -> IO ()
contextSetCiphers context list =
  withContext context $ \ctx ->
    withCString list $ \cpath ->
      _ssl_ctx_set_cipher_list ctx cpath >>= failIf_ (/= 1)

contextSetDefaultCiphers :: SSLContext -> IO ()
contextSetDefaultCiphers = flip contextSetCiphers "DEFAULT"

foreign import ccall unsafe "SSL_CTX_check_private_key"
   _ssl_ctx_check_private_key :: Ptr SSLContext_ -> IO CInt

-- | Return true iff the private key installed in the given context matches the
--   certificate also installed.
contextCheckPrivateKey :: SSLContext -> IO Bool
contextCheckPrivateKey context =
  withContext context $ \ctx ->
    fmap (== 1) (_ssl_ctx_check_private_key ctx)

-- | See <http://www.openssl.org/docs/ssl/SSL_CTX_set_verify.html>
data VerificationMode = VerifyNone
                      | VerifyPeer {
                          vpFailIfNoPeerCert :: Bool  -- ^ is a certificate required
                        , vpClientOnce       :: Bool  -- ^ only request once per connection
                        , vpCallback         :: Maybe (Bool -> X509StoreCtx -> IO Bool) -- ^ optional callback
                        }
                      deriving Typeable

foreign import ccall unsafe "SSL_CTX_set_verify"
   _ssl_set_verify_mode :: Ptr SSLContext_ -> CInt -> FunPtr VerifyCb -> IO ()

contextSetVerificationMode :: SSLContext -> VerificationMode -> IO ()
contextSetVerificationMode context VerifyNone =
  withContext context $ \ctx ->
    _ssl_set_verify_mode ctx (#const SSL_VERIFY_NONE) nullFunPtr >> return ()

contextSetVerificationMode context (VerifyPeer reqp oncep cbp) = do
  let mode = (#const SSL_VERIFY_PEER) .|.
             (if reqp then (#const SSL_VERIFY_FAIL_IF_NO_PEER_CERT) else 0) .|.
             (if oncep then (#const SSL_VERIFY_CLIENT_ONCE) else 0)
  withContext context $ \ctx -> mask_ $ do
    let cbRef = ctxVfCb context
    newCb <- mapM mkVerifyCb $ (<$> cbp) $ \cb pvf pStoreCtx ->
      cb pvf =<< wrapX509StoreCtx (return ()) pStoreCtx
    oldCb <- readIORef cbRef
    writeIORef cbRef newCb
    forM_ oldCb freeHaskellFunPtr
    _ssl_set_verify_mode ctx mode $ fromMaybe nullFunPtr newCb
    return ()

foreign import ccall unsafe "SSL_CTX_load_verify_locations"
  _ssl_load_verify_locations :: Ptr SSLContext_ -> Ptr CChar -> Ptr CChar -> IO CInt

-- | Set the location of a PEM encoded list of CA certificates to be used when
--   verifying a server's certificate
contextSetCAFile :: SSLContext -> FilePath -> IO ()
contextSetCAFile context path =
  withContext context $ \ctx ->
    withCString path $ \cpath ->
        _ssl_load_verify_locations ctx cpath nullPtr >>= failIf_ (/= 1)

-- | Set the path to a directory which contains the PEM encoded CA root
--   certificates. This is an alternative to 'contextSetCAFile'. See
--   <http://www.openssl.org/docs/ssl/SSL_CTX_load_verify_locations.html> for
--   details of the file naming scheme
contextSetCADirectory :: SSLContext -> FilePath -> IO ()
contextSetCADirectory context path =
  withContext context $ \ctx ->
    withCString path $ \cpath ->
        _ssl_load_verify_locations ctx nullPtr cpath >>= failIf_ (/= 1)

foreign import ccall unsafe "SSL_CTX_get_cert_store"
  _ssl_get_cert_store :: Ptr SSLContext_ -> IO (Ptr X509_STORE)

-- | Get a reference to, not a copy of, the X.509 certificate storage
--   in the SSL context.
contextGetCAStore :: SSLContext -> IO X509Store
contextGetCAStore context
    = withContext context $ \ ctx ->
      _ssl_get_cert_store ctx
           >>= wrapX509Store (touchContext context)


data SSL_
-- | This is the type of an SSL connection
--
--   IO with SSL objects is non-blocking and many SSL functions return a error
--   code which signifies that it needs to read or write more data. We handle
--   these calls and call threadWaitRead and threadWaitWrite at the correct
--   times. Thus multiple OS threads can be 'blocked' inside IO in the same SSL
--   object at a time, because they aren't really in the SSL object, they are
--   waiting for the RTS to wake the Haskell thread.
data SSL = SSL { sslCtx    :: SSLContext
               , sslMVar   :: MVar (Ptr SSL_)
               , sslFd     :: Fd -- ^ Get the underlying socket Fd
               , sslSocket :: Maybe Socket -- ^ Get the socket underlying an SSL connection
               }
           deriving Typeable

foreign import ccall unsafe "SSL_new" _ssl_new :: Ptr SSLContext_ -> IO (Ptr SSL_)
foreign import ccall unsafe "SSL_free" _ssl_free :: Ptr SSL_ -> IO ()
foreign import ccall unsafe "SSL_set_fd" _ssl_set_fd :: Ptr SSL_ -> CInt -> IO ()

connection' :: SSLContext -> Fd -> Maybe Socket -> IO SSL
connection' context fd@(Fd fdInt) sock = do
  mvar <- mask_ $ do
    ssl <- withContext context $ \ctx -> do
      ssl <- _ssl_new ctx >>= failIfNull
      _ssl_set_fd ssl fdInt
      return ssl
    mvar <- newMVar ssl
#if MIN_VERSION_base(4,6,0)
    _    <- mkWeakMVar mvar $ _ssl_free ssl
#else
    _    <- addMVarFinalizer mvar $ _ssl_free ssl
#endif
    return mvar
  return $ SSL { sslCtx    = context
               , sslMVar   = mvar
               , sslFd     = fd
               , sslSocket = sock
               }

-- | Wrap a Socket in an SSL connection. Reading and writing to the Socket
--   after this will cause weird errors in the SSL code. The SSL object
--   carries a handle to the Socket so you need not worry about the garbage
--   collector closing the file descriptor out from under you.
connection :: SSLContext -> Socket -> IO SSL
connection context sock@(MkSocket fd _ _ _ _) =
  connection' context (Fd fd) (Just sock)

-- | Wrap a socket Fd in an SSL connection.
fdConnection :: SSLContext -> Fd -> IO SSL
fdConnection context fd = connection' context fd Nothing

withSSL :: SSL -> (Ptr SSL_ -> IO a) -> IO a
withSSL = withMVar . sslMVar

foreign import ccall unsafe "HsOpenSSL_SSL_set_options"
    _SSL_set_options :: Ptr SSL_ -> CLong -> IO CLong

foreign import ccall unsafe "HsOpenSSL_SSL_clear_options"
    _SSL_clear_options :: Ptr SSL_ -> CLong -> IO CLong

-- | Add a protocol option to the SSL connection.
addOption :: SSL -> SSLOption -> IO ()
addOption ssl opt =
    withSSL ssl $ \sslPtr ->
        _SSL_set_options sslPtr (optionToIntegral opt) >> return ()

-- | Remove a protocol option from the SSL connection.
removeOption :: SSL -> SSLOption -> IO ()
removeOption ssl opt =
    withSSL ssl $ \sslPtr ->
        _SSL_clear_options sslPtr (optionToIntegral opt) >> return ()

foreign import ccall "SSL_accept" _ssl_accept :: Ptr SSL_ -> IO CInt
foreign import ccall "SSL_connect" _ssl_connect :: Ptr SSL_ -> IO CInt
foreign import ccall unsafe "SSL_get_error" _ssl_get_error :: Ptr SSL_ -> CInt -> IO CInt

throwSSLException :: String -> CInt -> IO a
throwSSLException loc ret
    = do e <- getError
         if e == 0 then
             case ret of
               0 -> throwIO ConnectionAbruptlyTerminated
               _ -> throwErrno loc
           else
             errorString e >>= throwIO . ProtocolError

-- | This is the type of an SSL IO operation. Errors are handled by
-- exceptions while everything else is one of these. Note that reading
-- from an SSL socket can result in WantWrite and vice versa.
data SSLResult a = SSLDone a  -- ^ operation finished successfully
                 | WantRead   -- ^ needs more data from the network
                 | WantWrite  -- ^ needs more outgoing buffer space
                 deriving (Eq, Show, Functor, Foldable, Traversable, Typeable)

-- | Block until the operation is finished.
sslBlock :: (SSL -> IO (SSLResult a)) -> SSL -> IO a
sslBlock action ssl
    = do result <- action ssl
         case result of
           SSLDone r -> return r
           WantRead  -> threadWaitRead  (sslFd ssl) >> sslBlock action ssl
           WantWrite -> threadWaitWrite (sslFd ssl) >> sslBlock action ssl

-- | Perform an SSL operation which can return non-blocking error codes, thus
--   requesting that the operation be performed when data or buffer space is
--   availible.
sslTryHandshake :: String
                -> (Ptr SSL_ -> IO CInt)
                -> SSL
                -> IO (SSLResult CInt)
sslTryHandshake loc action ssl
    = runInBoundThread $
      withSSL ssl $ \sslPtr ->
      do n <- action sslPtr
         if n == 1 then
             return $ SSLDone n
           else
             do err <- _ssl_get_error sslPtr n
                case err of
                  (#const SSL_ERROR_WANT_READ ) -> return WantRead
                  (#const SSL_ERROR_WANT_WRITE) -> return WantWrite
                  _ -> throwSSLException loc n

-- | Perform an SSL server handshake
accept :: SSL -> IO ()
accept = sslBlock tryAccept

-- | Try to perform an SSL server handshake without blocking
tryAccept :: SSL -> IO (SSLResult ())
tryAccept ssl
    = (() <$) <$> sslTryHandshake "SSL_accept" _ssl_accept ssl

-- | Perform an SSL client handshake
connect :: SSL -> IO ()
connect = sslBlock tryConnect

-- | Try to perform an SSL client handshake without blocking
tryConnect :: SSL -> IO (SSLResult ())
tryConnect ssl
    = (() <$) <$> sslTryHandshake "SSL_connect" _ssl_connect ssl

foreign import ccall "SSL_read" _ssl_read :: Ptr SSL_ -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall unsafe "SSL_get_shutdown" _ssl_get_shutdown :: Ptr SSL_ -> IO CInt

-- | Perform an SSL operation which operates of a buffer and can return
--   non-blocking error codes, thus requesting that it be performed again when
--   more data or buffer space is available.
--
--   Note that these SSL functions generally require that the arguments to the
--   repeated call be exactly the same. This presents an issue because multiple
--   threads could try writing at the same time (with different buffers) so the
--   calling function should probably hold the lock on the SSL object over the
--   whole time (include repeated calls)
sslIOInner :: String -- ^ the name of SSL IO function to call
           -> (Ptr SSL_ -> Ptr Word8 -> CInt -> IO CInt)  -- ^ the SSL IO function to call
           -> Ptr CChar  -- ^ the buffer to pass
           -> Int  -- ^ the length to pass
           -> SSL
           -> IO (SSLResult CInt)
sslIOInner loc f ptr nbytes ssl
    = runInBoundThread $
      withSSL ssl      $ \sslPtr ->
      do n <- f sslPtr (castPtr ptr) $ fromIntegral nbytes
         if n > 0 then
             return $ SSLDone $ fromIntegral n
           else
             do err <- _ssl_get_error sslPtr n
                case err of
                  (#const SSL_ERROR_ZERO_RETURN) -> return $ SSLDone $ 0
                  (#const SSL_ERROR_WANT_READ  ) -> return WantRead
                  (#const SSL_ERROR_WANT_WRITE ) -> return WantWrite
                  _ -> throwSSLException loc n

-- | Try to read the given number of bytes from an SSL connection. On EOF an
--   empty ByteString is returned. If the connection dies without a graceful
--   SSL shutdown, an exception is raised.
read :: SSL -> Int -> IO B.ByteString
read ssl nBytes = sslBlock (`tryRead` nBytes) ssl

-- | Try to read the given number of bytes from an SSL connection
--   without blocking.
tryRead :: SSL -> Int -> IO (SSLResult B.ByteString)
tryRead ssl nBytes
    = do (bs, result) <- B.createAndTrim' nBytes $ \bufPtr ->
                         do result <- sslIOInner "SSL_read" _ssl_read (castPtr bufPtr) nBytes ssl
                            case result of
                              SSLDone n -> return (0, fromIntegral n, SSLDone ())
                              WantRead  -> return (0,              0, WantRead  )
                              WantWrite -> return (0,              0, WantWrite )
         return $ bs <$ result

-- | Read some data into a raw pointer buffer.
-- Retrns the number of bytes read.
readPtr :: SSL -> Ptr a -> Int -> IO Int
readPtr ssl ptr len = sslBlock (\h -> tryReadPtr h ptr len) ssl

-- | Try to read some data into a raw pointer buffer, without blocking.
tryReadPtr :: SSL -> Ptr a -> Int -> IO (SSLResult Int)
tryReadPtr ssl bufPtr nBytes =
  fmap (fmap fromIntegral) (sslIOInner "SSL_read" _ssl_read (castPtr bufPtr) nBytes ssl)


foreign import ccall "SSL_write" _ssl_write :: Ptr SSL_ -> Ptr Word8 -> CInt -> IO CInt

-- | Write a given ByteString to the SSL connection. Either all the data is
--   written or an exception is raised because of an error.
write :: SSL -> B.ByteString -> IO ()
write ssl bs = sslBlock (`tryWrite` bs) ssl >> return ()

-- | Try to write a given ByteString to the SSL connection without blocking.
tryWrite :: SSL -> B.ByteString -> IO (SSLResult ())
tryWrite ssl bs
    | B.null bs = return $ SSLDone ()
    | otherwise
        = B.unsafeUseAsCStringLen bs $ \(ptr, len) -> tryWritePtr ssl ptr len

-- | Send some data from a raw pointer buffer.
writePtr :: SSL -> Ptr a -> Int -> IO ()
writePtr ssl ptr len = sslBlock (\h -> tryWritePtr h ptr len) ssl >> return ()

-- | Send some data from a raw pointer buffer, without blocking.
tryWritePtr :: SSL -> Ptr a -> Int -> IO (SSLResult ())
tryWritePtr ssl ptr len =
  do result <- sslIOInner "SSL_write" _ssl_write (castPtr ptr) len ssl
     case result of
       SSLDone 0 -> ioError $ errnoToIOError "SSL_write" ePIPE Nothing Nothing
       SSLDone _ -> return $ SSLDone ()
       WantRead  -> return WantRead
       WantWrite -> return WantWrite




-- | Lazily read all data until reaching EOF. If the connection dies
--   without a graceful SSL shutdown, an exception is raised.
lazyRead :: SSL -> IO L.ByteString
lazyRead ssl = fmap L.fromChunks lazyRead'
    where
      chunkSize = L.defaultChunkSize

      lazyRead' = unsafeInterleaveIO loop

      loop = do bs <- read ssl chunkSize
                if B.null bs then
                    -- got EOF
                    return []
                  else
                    do bss <- lazyRead'
                       return (bs:bss)

-- | Write a lazy ByteString to the SSL connection. In contrast to
--   'write', there is a chance that the string is written partway and
--   then an exception is raised for an error. The string doesn't
--   necessarily have to be finite.
lazyWrite :: SSL -> L.ByteString -> IO ()
lazyWrite ssl lbs
    = mapM_ (write ssl) $ L.toChunks lbs

foreign import ccall "SSL_shutdown" _ssl_shutdown :: Ptr SSL_ -> IO CInt

data ShutdownType = Bidirectional  -- ^ wait for the peer to also shutdown
                  | Unidirectional  -- ^ only send our shutdown
                  deriving (Eq, Show, Typeable)

-- | Cleanly shutdown an SSL connection. Note that SSL has a concept of a
--   secure shutdown, which is distinct from just closing the TCP connection.
--   This performs the former and should always be preferred.
--
--   This can either just send a shutdown, or can send and wait for the peer's
--   shutdown message.
shutdown :: SSL -> ShutdownType -> IO ()
shutdown ssl ty = sslBlock (`tryShutdown` ty) ssl

-- | Try to cleanly shutdown an SSL connection without blocking.
tryShutdown :: SSL -> ShutdownType -> IO (SSLResult ())
tryShutdown ssl ty = runInBoundThread $ withSSL ssl loop
    where
      loop :: Ptr SSL_ -> IO (SSLResult ())
      loop sslPtr
          = do n <- _ssl_shutdown sslPtr
               case n of
                 0 | ty == Bidirectional ->
                       -- We successfully sent a close notify alert to
                       -- the peer but haven't got a reply
                       -- yet. Complete the bidirectional shutdown by
                       -- calling SSL_shutdown(3) again.
                       loop sslPtr
                   | otherwise ->
                       -- Unidirection shutdown is enough for us.
                       return $ SSLDone ()
                 1 ->
                     -- Shutdown has succeeded, either bidirectionally
                     -- or unidirectionally.
                     return $ SSLDone ()
                 2 ->
                     -- SSL_shutdown(2) can return 2 according to its
                     -- documentation. It says we have to retry
                     -- calling SSL_shutdown(3) in this case.
                     loop sslPtr
                 _ -> do err <- _ssl_get_error sslPtr n
                         case err of
                           (#const SSL_ERROR_WANT_READ ) -> return WantRead
                           (#const SSL_ERROR_WANT_WRITE) -> return WantWrite
                           -- SSL_ERROR_SYSCALL/-1 happens when we are
                           -- trying to send the remote peer a "close
                           -- notify" alert but the underlying socket
                           -- was closed at the time. We don't treat
                           -- this an error /if and only if/ we have
                           -- already received a "close notify" from
                           -- the peer.
                           (#const SSL_ERROR_SYSCALL)
                               -> do sd <- _ssl_get_shutdown sslPtr
                                     if sd .&. (#const SSL_RECEIVED_SHUTDOWN) == 0 then
                                         throwSSLException "SSL_shutdown" n
                                       else
                                         return $ SSLDone ()
                           _   -> throwSSLException "SSL_shutdown" n

foreign import ccall "SSL_get_peer_certificate" _ssl_get_peer_cert :: Ptr SSL_ -> IO (Ptr X509_)

-- | After a successful connection, get the certificate of the other party. If
--   this is a server connection, you probably won't get a certificate unless
--   you asked for it with contextSetVerificationMode
getPeerCertificate :: SSL -> IO (Maybe X509)
getPeerCertificate ssl =
  withSSL ssl $ \ssl -> do
    cert <- _ssl_get_peer_cert ssl
    if cert == nullPtr
       then return Nothing
       else fmap Just (wrapX509 cert)

foreign import ccall "SSL_get_verify_result" _ssl_get_verify_result :: Ptr SSL_ -> IO CLong

-- | Get the result of verifing the peer's certificate. This is mostly for
--   clients to verify the certificate of the server that they have connected
--   it. You must set a list of root CA certificates with contextSetCA... for
--   this to make sense.
--
--   Note that this returns True iff the peer's certificate has a valid chain
--   to a root CA. You also need to check that the certificate is correct (i.e.
--   has the correct hostname in it) with getPeerCertificate.
getVerifyResult :: SSL -> IO Bool
getVerifyResult ssl =
  withSSL ssl $ \ssl -> do
    r <- _ssl_get_verify_result ssl
    return $ r == (#const X509_V_OK)

-- | The root exception type for all SSL exceptions.
data SomeSSLException
    = forall e. Exception e => SomeSSLException e
      deriving Typeable

instance Show SomeSSLException where
    show (SomeSSLException e) = show e

instance Exception SomeSSLException

sslExceptionToException :: Exception e => e -> SomeException
sslExceptionToException = toException . SomeSSLException

sslExceptionFromException :: Exception e => SomeException -> Maybe e
sslExceptionFromException x
    = do SomeSSLException a <- fromException x
         cast a

-- | The peer uncleanly terminated the connection without sending the
-- \"close notify\" alert.
data ConnectionAbruptlyTerminated
    = ConnectionAbruptlyTerminated
      deriving (Typeable, Show, Eq)

instance Exception ConnectionAbruptlyTerminated where
    toException   = sslExceptionToException
    fromException = sslExceptionFromException

-- | A failure in the SSL library occurred, usually a protocol
-- error.
data ProtocolError
    = ProtocolError !String
      deriving (Typeable, Show, Eq)

instance Exception ProtocolError where
    toException   = sslExceptionToException
    fromException = sslExceptionFromException

data SSLParam_

-- X509_VERIFY_PARAM *SSL_CTX_get0_param(SSL_CTX *ctx);
foreign import ccall unsafe "SSL_CTX_get0_param"
  sslGet0Param :: Ptr SSLContext_ -> IO (Ptr SSLParam_)

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
