{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_data_default_instances_dlist (
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
version = Version [0,0,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/superspole/.cabal/store/ghc-9.4.8/data-default-instances-dlist-0.0.1-17f4fc5757f9be5cc0f283881cd8ab022b31904ffd252a8f5ebf202daa296cff/bin"
libdir     = "/home/superspole/.cabal/store/ghc-9.4.8/data-default-instances-dlist-0.0.1-17f4fc5757f9be5cc0f283881cd8ab022b31904ffd252a8f5ebf202daa296cff/lib"
dynlibdir  = "/home/superspole/.cabal/store/ghc-9.4.8/data-default-instances-dlist-0.0.1-17f4fc5757f9be5cc0f283881cd8ab022b31904ffd252a8f5ebf202daa296cff/lib"
datadir    = "/home/superspole/.cabal/store/ghc-9.4.8/data-default-instances-dlist-0.0.1-17f4fc5757f9be5cc0f283881cd8ab022b31904ffd252a8f5ebf202daa296cff/share"
libexecdir = "/home/superspole/.cabal/store/ghc-9.4.8/data-default-instances-dlist-0.0.1-17f4fc5757f9be5cc0f283881cd8ab022b31904ffd252a8f5ebf202daa296cff/libexec"
sysconfdir = "/home/superspole/.cabal/store/ghc-9.4.8/data-default-instances-dlist-0.0.1-17f4fc5757f9be5cc0f283881cd8ab022b31904ffd252a8f5ebf202daa296cff/etc"

getBinDir     = catchIO (getEnv "data_default_instances_dlist_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "data_default_instances_dlist_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "data_default_instances_dlist_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "data_default_instances_dlist_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "data_default_instances_dlist_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "data_default_instances_dlist_sysconfdir") (\_ -> return sysconfdir)



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
