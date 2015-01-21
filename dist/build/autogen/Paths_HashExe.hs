module Paths_HashExe (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/roko/.cabal/bin"
libdir     = "/home/roko/.cabal/lib/HashExe-1.0/ghc-7.4.1"
datadir    = "/home/roko/.cabal/share/HashExe-1.0"
libexecdir = "/home/roko/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "HashExe_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HashExe_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HashExe_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HashExe_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
