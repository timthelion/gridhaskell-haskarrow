module Paths_Lit (
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

bindir     = "/home/timothy/.cabal/bin"
libdir     = "/home/timothy/.cabal/lib/Lit-1.0/ghc-7.4.1"
datadir    = "/home/timothy/.cabal/share/Lit-1.0"
libexecdir = "/home/timothy/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Lit_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Lit_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Lit_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Lit_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
