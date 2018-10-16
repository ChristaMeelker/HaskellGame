{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Game (
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

bindir     = "C:\\Users\\asus-pc\\Desktop\\Uni\\Informatica 2\\Functioneel Programmeren\\Game\\.cabal-sandbox\\bin"
libdir     = "C:\\Users\\asus-pc\\Desktop\\Uni\\Informatica 2\\Functioneel Programmeren\\Game\\.cabal-sandbox\\x86_64-windows-ghc-8.4.3\\Game-0.1.0.0-5Kz2JmUULc5BvhkKiM0Qj5"
dynlibdir  = "C:\\Users\\asus-pc\\Desktop\\Uni\\Informatica 2\\Functioneel Programmeren\\Game\\.cabal-sandbox\\x86_64-windows-ghc-8.4.3"
datadir    = "C:\\Users\\asus-pc\\Desktop\\Uni\\Informatica 2\\Functioneel Programmeren\\Game\\.cabal-sandbox\\x86_64-windows-ghc-8.4.3\\Game-0.1.0.0"
libexecdir = "C:\\Users\\asus-pc\\Desktop\\Uni\\Informatica 2\\Functioneel Programmeren\\Game\\.cabal-sandbox\\Game-0.1.0.0-5Kz2JmUULc5BvhkKiM0Qj5\\x86_64-windows-ghc-8.4.3\\Game-0.1.0.0"
sysconfdir = "C:\\Users\\asus-pc\\Desktop\\Uni\\Informatica 2\\Functioneel Programmeren\\Game\\.cabal-sandbox\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Game_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Game_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Game_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Game_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Game_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Game_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
