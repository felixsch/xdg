module System.Environment.XDG.Menu
  ( getUserMenuDir, getUserMenuDirWith
  , getMenuDirs, getMenuDirsWith
  ) where


import Control.Applicative

import System.FilePath ((</>))

import qualified Text.XML.Light as XML

import System.Environment.XDG.BaseDir


getUserMenuDir :: IO FilePath
getUserMenuDir
    = (</> "menus" </> "application.menu") <$> getUserConfigDir

getUserMenuDirWith :: String -> IO FilePath
getUserMenuDirWith
    de = (</> "menus" </> de ++ "-application.menu") <$> getUserConfigDir

getMenuDirs :: IO [FilePath]
getMenuDirs
    = map (</> "menu" </> "application.menu") <$> getConfigDirs


getMenuDirsWith :: String -> IO [FilePath]
getMenuDirsWith
    de = map (</> "menus" </> de ++ "-application.menu") <$> getConfigDirs
