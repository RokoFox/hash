module Paths_HashLib (
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
libdir     = "/home/roko/.cabal/lib/HashLib-1.0/ghc-7.4.1"
datadir    = "/home/roko/.cabal/share/HashLib-1.0"
libexecdir = "/home/roko/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "HashLib_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HashLib_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HashLib_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HashLib_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
