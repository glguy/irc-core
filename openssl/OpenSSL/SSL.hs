{-# LANGUAGE ForeignFunctionInterface #-}
module OpenSSL.SSL
    ( loadErrorStrings
    , addAllAlgorithms
    , libraryInit
    )
    where

foreign import ccall unsafe "SSL_load_error_strings"
        loadErrorStrings :: IO ()

foreign import ccall unsafe "HsOpenSSL_OpenSSL_add_all_algorithms"
        addAllAlgorithms :: IO ()

foreign import ccall unsafe "SSL_library_init"
        libraryInit :: IO ()
