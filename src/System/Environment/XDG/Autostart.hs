module System.Environment.XDG.Autostart
  ( getUserAutostartDir
  , getAutostartDirs
  , loadAutostartFiles
  ) where

import Control.Applicative

import qualified Data.Map as M

import System.FilePath
import System.Directory 

import System.Environment.XDG.BaseDir
import System.Environment.XDG.DesktopEntry


getUserAutostartDir :: IO FilePath
getUserAutostartDir
    = flip (</>) "autostart" <$> getUserConfigDir

getAutostartDirs :: IO [FilePath]
getAutostartDirs
    = map (</> "autostart") <$> getConfigDirs


loadAutostartFiles :: FilePath -> IO [DesktopEntry]
loadAutostartFiles
    path = (map filterH) <$> (mapM loadEntry =<< filterD <$> getDirectoryContents path)
    where
      filterH = filter (getValue "Hidden")
      filterD = filter isDesktopFile
      

isDesktopFile :: FilePath -> Bool
isDesktopFile
    file = ".desktop" == extension file
    where
        extension = dropWhile ('.' /=) . reverse . takeWhile ('/' /=) . reverse



