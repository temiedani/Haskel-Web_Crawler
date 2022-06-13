{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_crawler (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/temiedani/Desktop/QMUL/Functional Programming/Practice/crawler/.stack-work/install/x86_64-osx/d9108d48f799459300a1b7adf5b7be2fb0602dbf2532fd41bc5d9e3e4943e031/8.10.7/bin"
libdir     = "/Users/temiedani/Desktop/QMUL/Functional Programming/Practice/crawler/.stack-work/install/x86_64-osx/d9108d48f799459300a1b7adf5b7be2fb0602dbf2532fd41bc5d9e3e4943e031/8.10.7/lib/x86_64-osx-ghc-8.10.7/crawler-0.1.0.0-Cdl4WfiprqXx2Z4TSJUnA-crawler-exe"
dynlibdir  = "/Users/temiedani/Desktop/QMUL/Functional Programming/Practice/crawler/.stack-work/install/x86_64-osx/d9108d48f799459300a1b7adf5b7be2fb0602dbf2532fd41bc5d9e3e4943e031/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/temiedani/Desktop/QMUL/Functional Programming/Practice/crawler/.stack-work/install/x86_64-osx/d9108d48f799459300a1b7adf5b7be2fb0602dbf2532fd41bc5d9e3e4943e031/8.10.7/share/x86_64-osx-ghc-8.10.7/crawler-0.1.0.0"
libexecdir = "/Users/temiedani/Desktop/QMUL/Functional Programming/Practice/crawler/.stack-work/install/x86_64-osx/d9108d48f799459300a1b7adf5b7be2fb0602dbf2532fd41bc5d9e3e4943e031/8.10.7/libexec/x86_64-osx-ghc-8.10.7/crawler-0.1.0.0"
sysconfdir = "/Users/temiedani/Desktop/QMUL/Functional Programming/Practice/crawler/.stack-work/install/x86_64-osx/d9108d48f799459300a1b7adf5b7be2fb0602dbf2532fd41bc5d9e3e4943e031/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "crawler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "crawler_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "crawler_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "crawler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "crawler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "crawler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
