module System.Environment.XDG.Autostart
  ( getUserAutostartDir
  , getAutostartDirs
  , loadAutostartFiles
  , loadAutostartFilesWith
  ) where

import Control.Applicative

import Data.Maybe       (fromMaybe)

import System.FilePath  ((</>))
import System.Directory (getDirectoryContents)

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
    path = filter isHidden <$> (mapM loadEntry =<< filter isDesktopFile <$> getDirectoryContents path)


loadAutostartFilesWith :: String -> FilePath -> IO [DesktopEntry]
loadAutostartFilesWith
    de path = filter needToStart <$> loadAutostartFiles path
    where
        needToStart df = maybe (isNotShowIn df) (elem de) $ getValue "OnlyShowIn" df
        isNotShowIn = maybe False (notElem de) . getValue "NotShowIn"

isHidden :: DesktopEntry -> Bool
isHidden
    = fromMaybe False . getValue "Hidden"

isDesktopFile :: FilePath -> Bool
isDesktopFile
    file = ".desktop" == extension file
    where
        extension = dropWhile ('.' /=) . reverse . takeWhile ('/' /=) . reverse



