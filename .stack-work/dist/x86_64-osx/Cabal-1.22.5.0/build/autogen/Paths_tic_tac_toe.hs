module Paths_tic_tac_toe (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/rewinfrey/code/haskell/tic-tac-toe/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/bin"
libdir     = "/Users/rewinfrey/code/haskell/tic-tac-toe/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/lib/x86_64-osx-ghc-7.10.3/tic-tac-toe-0.1.0.0-DOSw0Xd6AgmHyP3uFUQ2Wv"
datadir    = "/Users/rewinfrey/code/haskell/tic-tac-toe/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/share/x86_64-osx-ghc-7.10.3/tic-tac-toe-0.1.0.0"
libexecdir = "/Users/rewinfrey/code/haskell/tic-tac-toe/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/libexec"
sysconfdir = "/Users/rewinfrey/code/haskell/tic-tac-toe/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tic_tac_toe_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tic_tac_toe_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "tic_tac_toe_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tic_tac_toe_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tic_tac_toe_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
