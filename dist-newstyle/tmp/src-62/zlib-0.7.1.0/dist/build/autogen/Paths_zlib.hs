{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_zlib (
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
version = Version [0,7,1,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/superspole/.cabal/store/ghc-9.4.8/zlib-0.7.1.0-ea2ccc2f93d951d84a88c78a7b4f196fea6a3780cdafe6f13025c03d6aaa9dce/bin"
libdir     = "/home/superspole/.cabal/store/ghc-9.4.8/zlib-0.7.1.0-ea2ccc2f93d951d84a88c78a7b4f196fea6a3780cdafe6f13025c03d6aaa9dce/lib"
dynlibdir  = "/home/superspole/.cabal/store/ghc-9.4.8/zlib-0.7.1.0-ea2ccc2f93d951d84a88c78a7b4f196fea6a3780cdafe6f13025c03d6aaa9dce/lib"
datadir    = "/home/superspole/.cabal/store/ghc-9.4.8/zlib-0.7.1.0-ea2ccc2f93d951d84a88c78a7b4f196fea6a3780cdafe6f13025c03d6aaa9dce/share"
libexecdir = "/home/superspole/.cabal/store/ghc-9.4.8/zlib-0.7.1.0-ea2ccc2f93d951d84a88c78a7b4f196fea6a3780cdafe6f13025c03d6aaa9dce/libexec"
sysconfdir = "/home/superspole/.cabal/store/ghc-9.4.8/zlib-0.7.1.0-ea2ccc2f93d951d84a88c78a7b4f196fea6a3780cdafe6f13025c03d6aaa9dce/etc"

getBinDir     = catchIO (getEnv "zlib_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "zlib_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "zlib_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "zlib_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "zlib_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "zlib_sysconfdir") (\_ -> return sysconfdir)



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
