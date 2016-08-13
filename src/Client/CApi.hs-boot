module Client.CApi where

import System.IO (FilePath)
import Foreign.Ptr (Ptr)

data ActiveExtension
activateExtension   :: Ptr () -> FilePath -> IO ActiveExtension
deactivateExtension :: Ptr () -> ActiveExtension -> IO ()
withStableMVar :: a -> (Ptr () -> IO b) -> IO (a,b)
