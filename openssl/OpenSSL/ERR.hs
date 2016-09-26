{-# LANGUAGE ForeignFunctionInterface #-}
module OpenSSL.ERR
    ( getError
    , peekError

    , errorString
    )
    where
import Foreign
import Foreign.C

foreign import ccall unsafe "ERR_get_error"
    getError :: IO CULong

foreign import ccall unsafe "ERR_peek_error"
    peekError :: IO CULong

foreign import ccall unsafe "ERR_error_string"
    _error_string :: CULong -> CString -> IO CString

errorString :: CULong -> IO String
errorString code
    = _error_string code nullPtr >>= peekCString
