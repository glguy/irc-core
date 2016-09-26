{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |An interface to PEM routines.
module OpenSSL.PEM
    ( -- * Password supply
      PemPasswordCallback
    , PemPasswordRWState(..)
    , PemPasswordSupply(..)

      -- * Private key
    , writePKCS8PrivateKey
    , readPrivateKey

      -- * Public key
    , writePublicKey
    , readPublicKey

      -- * X.509 certificate
    , writeX509
    , readX509

      -- * PKCS#10 certificate request
    , PemX509ReqFormat(..)
    , writeX509Req
    , readX509Req

      -- * Certificate Revocation List
    , writeCRL
    , readCRL

      -- * PKCS#7 structure
    , writePkcs7
    , readPkcs7

      -- * DH parameters
    , writeDHParams
    , readDHParams
    )
    where
import           Control.Exception hiding (try)
import           Control.Monad
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe
import           Foreign
import           Foreign.C
import           OpenSSL.BIO
import           OpenSSL.EVP.Cipher hiding (cipher)
import           OpenSSL.EVP.PKey
import           OpenSSL.EVP.Internal
import           OpenSSL.DH.Internal
import           OpenSSL.PKCS7
import           OpenSSL.Utils
import           OpenSSL.X509
import           OpenSSL.X509.Request
import           OpenSSL.X509.Revocation
#if !MIN_VERSION_base(4,6,0)
import           Prelude hiding (catch)
#endif
import           System.IO


-- |@'PemPasswordCallback'@ represents a callback function to supply a
-- password.
--
--   [@Int@] The maximum length of the password to be accepted.
--
--   [@PemPasswordRWState@] The context.
--
--   [@IO String@] The resulting password.
--
type PemPasswordCallback  = Int -> PemPasswordRWState -> IO String
type PemPasswordCallback' = Ptr CChar -> Int -> Int -> Ptr () -> IO Int


-- |@'PemPasswordRWState'@ represents a context of
-- 'PemPasswordCallback'.
data PemPasswordRWState = PwRead  -- ^ The callback was called to get
                                  --   a password to read something
                                  --   encrypted.
                        | PwWrite -- ^ The callback was called to get
                                  --   a password to encrypt
                                  --   something.

-- |@'PemPasswordSupply'@ represents a way to supply password.
--
-- FIXME: using PwTTY causes an error but I don't know why:
-- \"error:0906406D:PEM routines:DEF_CALLBACK:problems getting
-- password\"
data PemPasswordSupply = PwNone       -- ^ no password
                       | PwStr String -- ^ password in a static string
                       | PwBS B8.ByteString -- ^ password in a static bytestring.
                       | PwCallback PemPasswordCallback -- ^ get a
                                                        --   password
                                                        --   by a
                                                        --   callback
                       | PwTTY        -- ^ read a password from TTY


foreign import ccall "wrapper"
        mkPemPasswordCallback :: PemPasswordCallback' -> IO (FunPtr PemPasswordCallback')


rwflagToState :: Int -> PemPasswordRWState
rwflagToState 0 = PwRead
rwflagToState 1 = PwWrite
rwflagToState _ = undefined


callPasswordCB :: PemPasswordCallback -> PemPasswordCallback'
callPasswordCB cb buf bufLen rwflag _
    = let mode = rwflagToState rwflag
          try  = do passStr <- cb bufLen mode
                    let passLen = length passStr

                    when (passLen > bufLen)
                         $ failForTooLongPassword bufLen

                    pokeArray buf $ map (toEnum . fromEnum) passStr
                    return passLen
      in
        try `catch` \ exc ->
            do hPutStrLn stderr (show (exc :: SomeException))
               return 0 -- zero indicates an error
    where
      failForTooLongPassword :: Int -> IO a
      failForTooLongPassword len
          = fail ("callPasswordCB: the password which the callback returned is too long: "
                  ++ "it must be at most " ++ show len ++ " bytes.")


{- PKCS#8 -------------------------------------------------------------------- -}

foreign import ccall safe "PEM_write_bio_PKCS8PrivateKey"
        _write_bio_PKCS8PrivateKey :: Ptr BIO_
                                   -> Ptr EVP_PKEY
                                   -> Ptr EVP_CIPHER
                                   -> Ptr CChar
                                   -> CInt
                                   -> FunPtr PemPasswordCallback'
                                   -> Ptr a
                                   -> IO CInt

writePKCS8PrivateKey' :: KeyPair key =>
                         BIO
                      -> key
                      -> Maybe (Cipher, PemPasswordSupply)
                      -> IO ()
writePKCS8PrivateKey' bio key encryption
    = withBioPtr bio   $ \ bioPtr  ->
      withPKeyPtr' key $ \ pkeyPtr ->
      do ret <- case encryption of
                  Nothing
                      -> _write_bio_PKCS8PrivateKey bioPtr pkeyPtr nullPtr nullPtr 0 nullFunPtr nullPtr

                  Just (_, PwNone)
                      -> _write_bio_PKCS8PrivateKey bioPtr pkeyPtr nullPtr nullPtr 0 nullFunPtr nullPtr

                  Just (cipher, PwStr passStr)
                      -> withCStringLen passStr $ \(passPtr, passLen) ->
                         withCipherPtr cipher   $ \ cipherPtr          ->
                         _write_bio_PKCS8PrivateKey bioPtr pkeyPtr cipherPtr passPtr (fromIntegral passLen) nullFunPtr nullPtr
                  Just (cipher, PwBS passStr)
                      -> withBS passStr $ \(passPtr, passLen) ->
                         withCipherPtr cipher   $ \ cipherPtr          ->
                         _write_bio_PKCS8PrivateKey bioPtr pkeyPtr cipherPtr passPtr (fromIntegral passLen) nullFunPtr nullPtr
                  Just (cipher, PwCallback cb)
                      -> withCipherPtr cipher $ \ cipherPtr ->
                         bracket (mkPemPasswordCallback $ callPasswordCB cb) freeHaskellFunPtr $ \cbPtr ->
                         _write_bio_PKCS8PrivateKey bioPtr pkeyPtr cipherPtr nullPtr 0 cbPtr nullPtr
               
                  Just (cipher, PwTTY)
                      -> withCipherPtr cipher $ \ cipherPtr ->
                         _write_bio_PKCS8PrivateKey bioPtr pkeyPtr cipherPtr nullPtr 0 nullFunPtr nullPtr
         failIf_ (/= 1) ret

-- |@'writePKCS8PrivateKey'@ writes a private key to PEM string in
-- PKCS#8 format.
writePKCS8PrivateKey
    :: KeyPair key =>
       key       -- ^ private key to write
    -> Maybe (Cipher, PemPasswordSupply) -- ^ Either (symmetric cipher
                                         --   algorithm, password
                                         --   supply) or @Nothing@. If
                                         --   @Nothing@ is given the
                                         --   private key is not
                                         --   encrypted.
    -> IO String -- ^ the result PEM string
writePKCS8PrivateKey pkey encryption
    = do mem <- newMem
         writePKCS8PrivateKey' mem pkey encryption
         bioRead mem


foreign import ccall safe "PEM_read_bio_PrivateKey"
        _read_bio_PrivateKey :: Ptr BIO_
                             -> Ptr (Ptr EVP_PKEY)
                             -> FunPtr PemPasswordCallback'
                             -> CString
                             -> IO (Ptr EVP_PKEY)

readPrivateKey' :: BIO -> PemPasswordSupply -> IO SomeKeyPair
readPrivateKey' bio supply
    = withBioPtr bio $ \ bioPtr ->
      do pkeyPtr <- case supply of
                      PwNone
                          -> withCString "" $ \ strPtr ->
                             _read_bio_PrivateKey bioPtr nullPtr nullFunPtr (castPtr strPtr)
                      PwStr passStr
                          -> withCString passStr $
                             _read_bio_PrivateKey bioPtr nullPtr nullFunPtr
                      PwBS passStr
                          -> withBS passStr $ \(passPtr,_) ->
                             _read_bio_PrivateKey bioPtr nullPtr nullFunPtr passPtr
                      PwCallback cb
                          -> bracket (mkPemPasswordCallback $ callPasswordCB cb) freeHaskellFunPtr $ \cbPtr ->
                             _read_bio_PrivateKey bioPtr nullPtr cbPtr nullPtr
                      PwTTY
                          -> _read_bio_PrivateKey bioPtr nullPtr nullFunPtr nullPtr 
         failIfNull_ pkeyPtr
         fmap fromJust (wrapPKeyPtr pkeyPtr >>= fromPKey)

-- |@'readPrivateKey' pem supply@ reads a private key in PEM string.
readPrivateKey :: String -> PemPasswordSupply -> IO SomeKeyPair
readPrivateKey pemStr supply
    = do mem <- newConstMem pemStr
         readPrivateKey' mem supply


{- Public Key ---------------------------------------------------------------- -}

foreign import ccall unsafe "PEM_write_bio_PUBKEY"
        _write_bio_PUBKEY :: Ptr BIO_ -> Ptr EVP_PKEY -> IO CInt

foreign import ccall unsafe "PEM_read_bio_PUBKEY"
        _read_bio_PUBKEY :: Ptr BIO_
                         -> Ptr (Ptr EVP_PKEY)
                         -> FunPtr PemPasswordCallback'
                         -> Ptr ()
                         -> IO (Ptr EVP_PKEY)


writePublicKey' :: PublicKey key => BIO -> key -> IO ()
writePublicKey' bio key
    = withBioPtr bio   $ \ bioPtr  ->
      withPKeyPtr' key $ \ pkeyPtr ->
      _write_bio_PUBKEY bioPtr pkeyPtr >>= failIf (/= 1) >> return ()

-- |@'writePublicKey' pubkey@ writes a public to PEM string.
writePublicKey :: PublicKey key => key -> IO String
writePublicKey pkey
    = do mem <- newMem
         writePublicKey' mem pkey
         bioRead mem

-- Why the heck PEM_read_bio_PUBKEY takes pem_password_cb? Is there
-- any form of encrypted public key?
readPublicKey' :: BIO -> IO SomePublicKey
readPublicKey' bio
    = withBioPtr bio $ \ bioPtr ->
      withCString "" $ \ passPtr ->
      fmap fromJust
           ( _read_bio_PUBKEY bioPtr nullPtr nullFunPtr (castPtr passPtr)
             >>= failIfNull
             >>= wrapPKeyPtr
             >>= fromPKey
           )

-- |@'readPublicKey' pem@ reads a public key in PEM string.
readPublicKey :: String -> IO SomePublicKey
readPublicKey pemStr
    = newConstMem pemStr >>= readPublicKey'


{- X.509 certificate --------------------------------------------------------- -}

foreign import ccall unsafe "PEM_write_bio_X509"
        _write_bio_X509 :: Ptr BIO_
                        -> Ptr X509_
                        -> IO CInt

foreign import ccall safe "PEM_read_bio_X509"
        _read_bio_X509 :: Ptr BIO_
                       -> Ptr (Ptr X509_)
                       -> FunPtr PemPasswordCallback'
                       -> Ptr ()
                       -> IO (Ptr X509_)

writeX509' :: BIO -> X509 -> IO ()
writeX509' bio x509
    = withBioPtr bio   $ \ bioPtr  ->
      withX509Ptr x509 $ \ x509Ptr ->
      _write_bio_X509 bioPtr x509Ptr
           >>= failIf (/= 1)
           >>  return ()

-- |@'writeX509' cert@ writes an X.509 certificate to PEM string.
writeX509 :: X509 -> IO String
writeX509 x509
    = do mem <- newMem
         writeX509' mem x509
         bioRead mem


-- I believe X.509 isn't encrypted.
readX509' :: BIO -> IO X509
readX509' bio
    = withBioPtr bio $ \ bioPtr ->
      withCString "" $ \ passPtr ->
      _read_bio_X509 bioPtr nullPtr nullFunPtr (castPtr passPtr)
           >>= failIfNull
           >>= wrapX509

-- |@'readX509' pem@ reads an X.509 certificate in PEM string.
readX509 :: String -> IO X509
readX509 pemStr
    = newConstMem pemStr >>= readX509'


{- PKCS#10 certificate request ----------------------------------------------- -}

foreign import ccall unsafe "PEM_write_bio_X509_REQ"
        _write_bio_X509_REQ :: Ptr BIO_
                            -> Ptr X509_REQ
                            -> IO CInt

foreign import ccall unsafe "PEM_write_bio_X509_REQ_NEW"
        _write_bio_X509_REQ_NEW :: Ptr BIO_
                                -> Ptr X509_REQ
                                -> IO CInt

foreign import ccall safe "PEM_read_bio_X509_REQ"
        _read_bio_X509_REQ :: Ptr BIO_
                           -> Ptr (Ptr X509_REQ)
                           -> FunPtr PemPasswordCallback'
                           -> Ptr ()
                           -> IO (Ptr X509_REQ)

-- |@'PemX509ReqFormat'@ represents format of PKCS#10 certificate
-- request.
data PemX509ReqFormat
    = ReqNewFormat -- ^ The new format, whose header is \"NEW
                   --   CERTIFICATE REQUEST\".
    | ReqOldFormat -- ^ The old format, whose header is \"CERTIFICATE
                   --   REQUEST\".


writeX509Req' :: BIO -> X509Req -> PemX509ReqFormat -> IO ()
writeX509Req' bio req format
    = withBioPtr bio     $ \ bioPtr ->
      withX509ReqPtr req $ \ reqPtr ->
      writer bioPtr reqPtr
                 >>= failIf (/= 1)
                 >>  return ()
    where
      writer = case format of
                 ReqNewFormat -> _write_bio_X509_REQ_NEW
                 ReqOldFormat -> _write_bio_X509_REQ

-- |@'writeX509Req'@ writes a PKCS#10 certificate request to PEM
-- string.
writeX509Req :: X509Req          -- ^ request
             -> PemX509ReqFormat -- ^ format
             -> IO String        -- ^ the result PEM string
writeX509Req req format
    = do mem <- newMem
         writeX509Req' mem req format
         bioRead mem


readX509Req' :: BIO -> IO X509Req
readX509Req' bio
    = withBioPtr bio $ \ bioPtr ->
      withCString "" $ \ passPtr ->
      _read_bio_X509_REQ bioPtr nullPtr nullFunPtr (castPtr passPtr)
           >>= failIfNull
           >>= wrapX509Req

-- |@'readX509Req'@ reads a PKCS#10 certificate request in PEM string.
readX509Req :: String -> IO X509Req
readX509Req pemStr
    = newConstMem pemStr >>= readX509Req'


{- Certificate Revocation List ----------------------------------------------- -}

foreign import ccall unsafe "PEM_write_bio_X509_CRL"
        _write_bio_X509_CRL :: Ptr BIO_
                            -> Ptr X509_CRL
                            -> IO CInt

foreign import ccall safe "PEM_read_bio_X509_CRL"
        _read_bio_X509_CRL :: Ptr BIO_
                           -> Ptr (Ptr X509_CRL)
                           -> FunPtr PemPasswordCallback'
                           -> Ptr ()
                           -> IO (Ptr X509_CRL)


writeCRL' :: BIO -> CRL -> IO ()
writeCRL' bio crl
    = withBioPtr bio $ \ bioPtr ->
      withCRLPtr crl $ \ crlPtr ->
      _write_bio_X509_CRL bioPtr crlPtr
           >>= failIf (/= 1)
           >>  return ()

-- |@'writeCRL' crl@ writes a Certificate Revocation List to PEM
-- string.
writeCRL :: CRL -> IO String
writeCRL crl
    = do mem <- newMem
         writeCRL' mem crl
         bioRead mem


readCRL' :: BIO -> IO CRL
readCRL' bio
    = withBioPtr bio $ \ bioPtr ->
      withCString "" $ \ passPtr ->
      _read_bio_X509_CRL bioPtr nullPtr nullFunPtr (castPtr passPtr)
           >>= failIfNull
           >>= wrapCRL

-- |@'readCRL' pem@ reads a Certificate Revocation List in PEM string.
readCRL :: String -> IO CRL
readCRL pemStr
    = newConstMem pemStr >>= readCRL'


{- PKCS#7 -------------------------------------------------------------------- -}

foreign import ccall unsafe "PEM_write_bio_PKCS7"
        _write_bio_PKCS7 :: Ptr BIO_
                         -> Ptr PKCS7
                         -> IO CInt

foreign import ccall safe "PEM_read_bio_PKCS7"
        _read_bio_PKCS7 :: Ptr BIO_
                        -> Ptr (Ptr PKCS7)
                        -> FunPtr PemPasswordCallback'
                        -> Ptr ()
                        -> IO (Ptr PKCS7)


writePkcs7' :: BIO -> Pkcs7 -> IO ()
writePkcs7' bio pkcs7
    = withBioPtr bio     $ \ bioPtr ->
      withPkcs7Ptr pkcs7 $ \ pkcs7Ptr ->
      _write_bio_PKCS7 bioPtr pkcs7Ptr
           >>= failIf (/= 1)
           >>  return ()

-- |@'writePkcs7' p7@ writes a PKCS#7 structure to PEM string.
writePkcs7 :: Pkcs7 -> IO String
writePkcs7 pkcs7
    = do mem <- newMem
         writePkcs7' mem pkcs7
         bioRead mem


readPkcs7' :: BIO -> IO Pkcs7
readPkcs7' bio
    = withBioPtr bio $ \ bioPtr ->
      withCString "" $ \ passPtr ->
      _read_bio_PKCS7 bioPtr nullPtr nullFunPtr (castPtr passPtr)
           >>= failIfNull
           >>= wrapPkcs7Ptr

-- |@'readPkcs7' pem@ reads a PKCS#7 structure in PEM string.
readPkcs7 :: String -> IO Pkcs7
readPkcs7 pemStr
    = newConstMem pemStr >>= readPkcs7'

{- DH parameters ------------------------------------------------------------- -}

foreign import ccall unsafe "PEM_write_bio_DHparams"
        _write_bio_DH :: Ptr BIO_
                      -> Ptr DH_
                      -> IO CInt

foreign import ccall safe "PEM_read_bio_DHparams"
        _read_bio_DH :: Ptr BIO_
                     -> Ptr (Ptr DH_)
                     -> FunPtr PemPasswordCallback'
                     -> Ptr ()
                     -> IO (Ptr DH_)

writeDHParams' :: BIO -> DHP -> IO ()
writeDHParams' bio dh
    = withBioPtr bio $ \ bioPtr ->
      withDHPPtr dh  $ \ dhPtr ->
        _write_bio_DH bioPtr dhPtr >>= failIf_ (/= 1)

-- |@'writeDHParams' dh@ writes DH parameters to PEM string.
writeDHParams :: DHP -> IO String
writeDHParams dh
    = do mem <- newMem
         writeDHParams' mem dh
         bioRead mem

readDHParams' :: BIO -> IO DHP
readDHParams' bio
    = withBioPtr bio $ \ bioPtr ->
      withCString "" $ \ passPtr ->
        _read_bio_DH bioPtr nullPtr nullFunPtr (castPtr passPtr)
          >>= failIfNull
          >>= wrapDHPPtr

-- |@'readDHParams' pem@ reads DH parameters in PEM string.
readDHParams :: String -> IO DHP
readDHParams pemStr
    = newConstMem pemStr >>= readDHParams'


withBS :: B8.ByteString -> ((Ptr CChar, Int) -> IO t) -> IO t
withBS passStr act =
  B8.useAsCStringLen passStr $ \ (passPtr, passLen) ->
  flip finally (memset passPtr 0 $ fromIntegral passLen) $
  act (castPtr passPtr, passLen)

foreign import ccall unsafe memset :: Ptr a -> CInt -> CSize -> IO ()
