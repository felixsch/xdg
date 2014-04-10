module System.Environment.XDG.BaseDir
    ( getUserDataDir
    , getUserConfigDir
    , getUserCacheDir
    , getDataDirs
    , getConfigDirs
    , getRuntimeDir
    ) where

import Control.Applicative

import System.Environment
import System.FilePath

import Data.Maybe


fromEnv :: String -> IO FilePath
fromEnv key = fromMaybe [] <$> lookupEnv key


getDirWith :: String -> FilePath -> IO FilePath
getDirWith
    key def = check <$> fromEnv key
    where
        check [] = def
        check x  = x

getDirsWith :: String -> [FilePath] -> IO [FilePath]
getDirsWith
    key def = check . split (== ':') <$> fromEnv key
    where
        check [] = def
        check x  = x

getHomeDir :: IO FilePath
getHomeDir
    = getEnv "HOME"

split     :: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'

userR :: FilePath -> IO FilePath
userR
    path = (</>) <$> getHomeDir <*> return path


getUserDataDir :: IO FilePath
getUserDataDir
    = getDirWith "XDG_DATA_HOME" =<< userR ".local/share"

getUserConfigDir :: IO FilePath
getUserConfigDir
    = getDirWith "XDG_CONFIG_HOME" =<< userR ".config"

getUserCacheDir :: IO FilePath
getUserCacheDir
    = getDirWith "XDG_CACHE_HOME" =<< userR ".cache"


getDataDirs :: IO [FilePath]
getDataDirs
    = getDirsWith "XDG_DATA_DIRS" ["/usr/local/share", "/usr/share"]

getConfigDirs :: IO [FilePath]
getConfigDirs
    = getDirsWith "XDG_CONFIG_DIRS" ["/etx/xdg"]


getRuntimeDir :: IO (Maybe FilePath)
getRuntimeDir
    = lookupEnv "XDG_RUNTIME_DIR"
