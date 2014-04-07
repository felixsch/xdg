module System.Environment.XDG.MimeApps
    ( MimeApps
    , loadMimeApps, saveMimeApps
    , getAssociation, setAssociation
    , getDefault, setDefault
    , addDefault
    , getDefaultApps
    )
    where

import Prelude
import Control.Applicative
import qualified Data.Map as M

import System.Environment.XDG.Internal.Ini


type MimeApps = IniFile


loadMimeApps :: FilePath -> IO MimeApps
loadMimeApps
    path = check =<< decodeIni <$> readFile path
    where
        check (Left err) = error $ "Could not load mimeapps file: " ++ err
        check (Right x)  = return x

saveMimeApps :: String -> MimeApps -> IO ()
saveMimeApps
    path = writeFile path . encodeIni

getAssociation :: String -> MimeApps -> Maybe String
getAssociation
    = getKey "Added Associations"

setAssociation :: String -> String -> MimeApps -> MimeApps
setAssociation
    = setKey "AddedAssociations"

getDefault :: String -> MimeApps -> Maybe String
getDefault
    = getKey "Default Applications"

setDefault :: String -> String -> MimeApps -> MimeApps
setDefault
    = setKey "Default Applications"

addDefault :: String -> String -> MimeApps -> MimeApps
addDefault
    sec val ini = setKey "Added Associations" sec val $ setKey "DefaultApplications" sec val ini

getDefaultApps :: MimeApps -> Maybe [(String, IniValue)]
getDefaultApps
    apps = M.toList <$> getSection "Default Applications" apps
