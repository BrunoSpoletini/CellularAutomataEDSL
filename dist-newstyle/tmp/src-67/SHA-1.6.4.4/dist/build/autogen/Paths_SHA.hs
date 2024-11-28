{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_SHA (
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
version = Version [1,6,4,4] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/superspole/.cabal/store/ghc-9.4.8/SHA-1.6.4.4-8ccdd1f80bd28bf760dc07816bb78841e6cfb732f5cde9d8ab00b94ba8ff2ae9/bin"
libdir     = "/home/superspole/.cabal/store/ghc-9.4.8/SHA-1.6.4.4-8ccdd1f80bd28bf760dc07816bb78841e6cfb732f5cde9d8ab00b94ba8ff2ae9/lib"
dynlibdir  = "/home/superspole/.cabal/store/ghc-9.4.8/SHA-1.6.4.4-8ccdd1f80bd28bf760dc07816bb78841e6cfb732f5cde9d8ab00b94ba8ff2ae9/lib"
datadir    = "/home/superspole/.cabal/store/ghc-9.4.8/SHA-1.6.4.4-8ccdd1f80bd28bf760dc07816bb78841e6cfb732f5cde9d8ab00b94ba8ff2ae9/share"
libexecdir = "/home/superspole/.cabal/store/ghc-9.4.8/SHA-1.6.4.4-8ccdd1f80bd28bf760dc07816bb78841e6cfb732f5cde9d8ab00b94ba8ff2ae9/libexec"
sysconfdir = "/home/superspole/.cabal/store/ghc-9.4.8/SHA-1.6.4.4-8ccdd1f80bd28bf760dc07816bb78841e6cfb732f5cde9d8ab00b94ba8ff2ae9/etc"

getBinDir     = catchIO (getEnv "SHA_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "SHA_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "SHA_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "SHA_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SHA_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SHA_sysconfdir") (\_ -> return sysconfdir)



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
