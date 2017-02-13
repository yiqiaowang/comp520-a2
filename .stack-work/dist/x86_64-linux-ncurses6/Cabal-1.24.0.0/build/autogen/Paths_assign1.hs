{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_assign1 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/yiqiao/MEGAsync/winter-2017/c520/assignments/assign2/.stack-work/install/x86_64-linux-ncurses6/lts-7.15/8.0.1/bin"
libdir     = "/home/yiqiao/MEGAsync/winter-2017/c520/assignments/assign2/.stack-work/install/x86_64-linux-ncurses6/lts-7.15/8.0.1/lib/x86_64-linux-ghc-8.0.1/assign1-0.1.0.0-AYX7x0xw8taFPfaDIqX2Ru"
datadir    = "/home/yiqiao/MEGAsync/winter-2017/c520/assignments/assign2/.stack-work/install/x86_64-linux-ncurses6/lts-7.15/8.0.1/share/x86_64-linux-ghc-8.0.1/assign1-0.1.0.0"
libexecdir = "/home/yiqiao/MEGAsync/winter-2017/c520/assignments/assign2/.stack-work/install/x86_64-linux-ncurses6/lts-7.15/8.0.1/libexec"
sysconfdir = "/home/yiqiao/MEGAsync/winter-2017/c520/assignments/assign2/.stack-work/install/x86_64-linux-ncurses6/lts-7.15/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "assign1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "assign1_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "assign1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "assign1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "assign1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
