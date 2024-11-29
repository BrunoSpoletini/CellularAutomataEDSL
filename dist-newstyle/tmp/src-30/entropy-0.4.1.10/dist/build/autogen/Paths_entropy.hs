{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_entropy (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,4,1,10] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/superspole/.cabal/store/ghc-9.4.8/entropy-0.4.1.10-8a88a498a3649f2005dd29fe3895ea0b4a38deb928a7cde6a3d75056a9bc8a47/bin"
libdir     = "/home/superspole/.cabal/store/ghc-9.4.8/entropy-0.4.1.10-8a88a498a3649f2005dd29fe3895ea0b4a38deb928a7cde6a3d75056a9bc8a47/lib"
dynlibdir  = "/home/superspole/.cabal/store/ghc-9.4.8/entropy-0.4.1.10-8a88a498a3649f2005dd29fe3895ea0b4a38deb928a7cde6a3d75056a9bc8a47/lib"
datadir    = "/home/superspole/.cabal/store/ghc-9.4.8/entropy-0.4.1.10-8a88a498a3649f2005dd29fe3895ea0b4a38deb928a7cde6a3d75056a9bc8a47/share"
libexecdir = "/home/superspole/.cabal/store/ghc-9.4.8/entropy-0.4.1.10-8a88a498a3649f2005dd29fe3895ea0b4a38deb928a7cde6a3d75056a9bc8a47/libexec"
sysconfdir = "/home/superspole/.cabal/store/ghc-9.4.8/entropy-0.4.1.10-8a88a498a3649f2005dd29fe3895ea0b4a38deb928a7cde6a3d75056a9bc8a47/etc"

getBinDir     = catchIO (getEnv "entropy_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "entropy_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "entropy_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "entropy_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "entropy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "entropy_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
