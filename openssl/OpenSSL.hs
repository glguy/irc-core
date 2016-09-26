{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |HsOpenSSL is an OpenSSL binding for Haskell. It can generate RSA
-- and DSA keys, read and write PEM files, generate message digests,
-- sign and verify messages, encrypt and decrypt messages.
--
-- Please note that this project has started at the time when there
-- were no pure-Haskell implementations of TLS. Now there is tls
-- package (<http://hackage.haskell.org/package/tls>), which looks
-- pretty saner than HsOpenSSL especially for initialisation and error
-- handlings. So PHO (the initial author of HsOpenSSL) wants to
-- encourage you to use and improve the tls package instead as long as
-- possible. The only problem is that the tls package has not received
-- as much review as OpenSSL from cryptography specialists yet, thus
-- we can't assume it's secure enough.
--
-- Features that aren't (yet) supported:
--
--   [/SSL network connection/] ssl(3) functionalities aren't fully
--   covered yet. See "OpenSSL.Session".
--
--   [/Complete coverage of Low-level API to symmetric ciphers/] Only
--   high-level APIs (EVP and BIO) are fully available. But I believe
--   no one will be lost without functions like @DES_set_odd_parity@.
--
--   [/Low-level API to asymmetric ciphers/] Only a high-level API
--   (EVP) is available. But I believe no one will complain about the
--   absence of functions like @RSA_public_encrypt@.
--
--   [/X.509 v3 extension handling/] It should be supported in the
--   future.
--
--   [/Low-level API to message digest functions/] Just use EVP
--   instead of something like @MD5_Update@.
--
--   [/API to PKCS#12 functionality/] It should be covered someday.
--
--   [/BIO/] BIO isn't needed because we are Haskell hackers. Though
--   HsOpenSSL itself uses BIO internally.
--
--   [/ENGINE cryptographic module/] The default implementations work
--   very well, don't they?
module OpenSSL
    ( withOpenSSL
    )
    where
import Control.Concurrent.MVar
import Control.Monad
import OpenSSL.SSL
import System.IO.Unsafe

#if !MIN_VERSION_base(4,6,0)
import Control.Exception (onException, mask_)
#endif

foreign import ccall "HsOpenSSL_setupMutex"
        setupMutex :: IO ()


-- |Computation of @'withOpenSSL' action@ initializes the OpenSSL
-- library as necessary, and computes @action@. Every application that
-- uses HsOpenSSL must wrap any operations involving OpenSSL with
-- 'withOpenSSL', or they might crash:
--
-- > module Main where
-- > import OpenSSL
-- >
-- > main :: IO ()
-- > main = withOpenSSL $
-- >        do ...
--
-- Since 0.10.3.5, 'withOpenSSL' is safe to be applied
-- redundantly. Library authors may wish to wrap their functions not
-- to force their users to think about initialization:
--
-- > get :: URI -> IO Response
-- > get uri = withOpenSSL $ internalImplementationOfGet uri
--
withOpenSSL :: IO a -> IO a
withOpenSSL io
    -- We don't want our initialisation sequence to be interrupted
    -- halfway.
    = do modifyMVarMasked_ isInitialised $ \ done ->
             do unless done $ do loadErrorStrings
                                 addAllAlgorithms
                                 libraryInit
                                 setupMutex
                return True
         io

#if !MIN_VERSION_base(4,6,0)
{-|
  Like 'modifyMVar_', but the @IO@ action in the second argument is executed with
  asynchronous exceptions masked.
-}
{-# INLINE modifyMVarMasked_ #-}
modifyMVarMasked_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVarMasked_ m io =
  mask_ $ do
    a  <- takeMVar m
    a' <- io a `onException` putMVar m a
    putMVar m a'
#endif

-- This variable must be atomically fetched/stored not to initialise
-- the library twice.
isInitialised :: MVar Bool
{-# NOINLINE isInitialised #-}
isInitialised =
    unsafePerformIO $ newMVar False
