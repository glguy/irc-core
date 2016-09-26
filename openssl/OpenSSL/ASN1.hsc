{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module OpenSSL.ASN1
    ( ASN1_OBJECT
    , obj2nid
    , nid2sn
    , nid2ln

    , ASN1_STRING
    , peekASN1String

    , ASN1_INTEGER
    , peekASN1Integer
    , withASN1Integer

    , ASN1_TIME
    , peekASN1Time
    , withASN1Time
    )
    where
#include "HsOpenSSL.h"
import           Control.Exception
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Foreign
import           Foreign.C
import           OpenSSL.BIO
import           OpenSSL.BN
import           OpenSSL.Utils
#if !MIN_VERSION_time(1,5,0)
import           System.Locale
#endif

{- ASN1_OBJECT --------------------------------------------------------------- -}

data ASN1_OBJECT

foreign import ccall unsafe "OBJ_obj2nid"
        obj2nid :: Ptr ASN1_OBJECT -> IO CInt

foreign import ccall unsafe "OBJ_nid2sn"
        _nid2sn :: CInt -> IO CString

foreign import ccall unsafe "OBJ_nid2ln"
        _nid2ln :: CInt -> IO CString


nid2sn :: CInt -> IO String
nid2sn nid = _nid2sn nid >>= peekCString


nid2ln :: CInt -> IO String
nid2ln nid = _nid2ln nid >>= peekCString


{- ASN1_STRING --------------------------------------------------------------- -}

data ASN1_STRING

peekASN1String :: Ptr ASN1_STRING -> IO String
peekASN1String strPtr
    = do buf <- (#peek ASN1_STRING, data  ) strPtr
         len <- (#peek ASN1_STRING, length) strPtr :: IO CInt
         peekCStringLen (buf, fromIntegral len)


{- ASN1_INTEGER -------------------------------------------------------------- -}

data ASN1_INTEGER

foreign import ccall unsafe "HsOpenSSL_M_ASN1_INTEGER_new"
        _ASN1_INTEGER_new :: IO (Ptr ASN1_INTEGER)

foreign import ccall unsafe "HsOpenSSL_M_ASN1_INTEGER_free"
        _ASN1_INTEGER_free :: Ptr ASN1_INTEGER -> IO ()

foreign import ccall unsafe "ASN1_INTEGER_to_BN"
        _ASN1_INTEGER_to_BN :: Ptr ASN1_INTEGER -> Ptr BIGNUM -> IO (Ptr BIGNUM)

foreign import ccall unsafe "BN_to_ASN1_INTEGER"
        _BN_to_ASN1_INTEGER :: Ptr BIGNUM -> Ptr ASN1_INTEGER -> IO (Ptr ASN1_INTEGER)


peekASN1Integer :: Ptr ASN1_INTEGER -> IO Integer
peekASN1Integer intPtr
    = allocaBN $ \ bn ->
      do _ASN1_INTEGER_to_BN intPtr (unwrapBN bn)
              >>= failIfNull_
         peekBN bn


allocaASN1Integer :: (Ptr ASN1_INTEGER -> IO a) -> IO a
allocaASN1Integer
    = bracket _ASN1_INTEGER_new _ASN1_INTEGER_free


withASN1Integer :: Integer -> (Ptr ASN1_INTEGER -> IO a) -> IO a
withASN1Integer int m
    = withBN int $ \ bn ->
      allocaASN1Integer $ \ intPtr ->
      do _BN_to_ASN1_INTEGER (unwrapBN bn) intPtr
              >>= failIfNull_
         m intPtr


{- ASN1_TIME ---------------------------------------------------------------- -}

data ASN1_TIME

foreign import ccall unsafe "HsOpenSSL_M_ASN1_TIME_new"
        _ASN1_TIME_new :: IO (Ptr ASN1_TIME)

foreign import ccall unsafe "HsOpenSSL_M_ASN1_TIME_free"
        _ASN1_TIME_free :: Ptr ASN1_TIME -> IO ()

foreign import ccall unsafe "ASN1_TIME_set"
        _ASN1_TIME_set :: Ptr ASN1_TIME -> CTime -> IO (Ptr ASN1_TIME)

foreign import ccall unsafe "ASN1_TIME_print"
        _ASN1_TIME_print :: Ptr BIO_ -> Ptr ASN1_TIME -> IO CInt


peekASN1Time :: Ptr ASN1_TIME -> IO UTCTime -- asn1/t_x509.c
peekASN1Time time
    = do bio <- newMem
         withBioPtr bio $ \ bioPtr ->
             _ASN1_TIME_print bioPtr time
                  >>= failIf_ (/= 1)
         timeStr <- bioRead bio
#if MIN_VERSION_time(1,5,0)
         case parseTimeM True locale "%b %e %H:%M:%S %Y %Z" timeStr of
#else
         case parseTime locale "%b %e %H:%M:%S %Y %Z" timeStr of
#endif
           Just utc -> return utc
           Nothing  -> fail ("peekASN1Time: failed to parse time string: " ++ timeStr)
    where
      locale :: TimeLocale
      locale = TimeLocale {
                 wDays       = undefined
               , months      = [ (undefined, x)
                                     | x <- [ "Jan", "Feb", "Mar", "Apr", "May", "Jun"
                                            , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
                                            ]
                               ]
#if !MIN_VERSION_time(1,5,0)
               , intervals   = undefined
#endif
               , amPm        = undefined
               , dateTimeFmt = undefined
               , dateFmt     = undefined
               , timeFmt     = undefined
               , time12Fmt   = undefined
#if MIN_VERSION_time(1,5,0)
               , knownTimeZones = []
#endif
               }


allocaASN1Time :: (Ptr ASN1_TIME -> IO a) -> IO a
allocaASN1Time
    = bracket _ASN1_TIME_new _ASN1_TIME_free


withASN1Time :: UTCTime -> (Ptr ASN1_TIME -> IO a) -> IO a
withASN1Time utc m
    = allocaASN1Time $ \ time ->
      do _ASN1_TIME_set time (fromIntegral (round $ utcTimeToPOSIXSeconds utc :: Integer))
              >>= failIfNull_
         m time