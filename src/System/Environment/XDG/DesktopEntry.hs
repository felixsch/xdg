{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module System.Environment.XDG.DesktopEntry
    ( EntryType(..)
    , DesktopEntry(..)
    , LocaleString
    , newEntry
    , loadEntry , saveEntry , saveEntryWith
    , getName , setName
    , getGenericName , setGenericName
    , setPath, getPath
    , getType , setType
    , getTerminal , setTerminal
    , getIcon , setIcon
    , getExec , setExec
    , getUrl , setUrl
    , getVersion , setVersion
    , execEntry
    , execEntryWith
    , getValue
    , setValue
    , typeFromText
    , getC , setC
    , getLang , setLang )
    where

import Prelude
import Control.Applicative
import Data.Maybe
import qualified Data.Map as M
import System.Process

import System.Environment.XDG.Internal.Ini

data DesktopEntry = DesktopEntry FilePath IniFile
type LocaleString = M.Map String String

data EntryType = Application | Directory | Link | Unknown String
    deriving (Show, Eq)

typeFromText :: String -> EntryType
typeFromText "Application" = Application
typeFromText "Directory"   = Directory
typeFromText "Link"        = Link
typeFromText x             = Unknown x


getValue :: (CastValue a) => String -> DesktopEntry -> Maybe a
getValue 
    key (DesktopEntry _ ini) = getKey "Desktop Entry" key ini

setValue :: (CastValue a) => String -> a -> DesktopEntry -> DesktopEntry
setValue 
    key val (DesktopEntry path ini) = DesktopEntry path $ setKey "Desktop Entry" key val ini

getC :: LocaleString -> String
getC = fromJust . M.lookup "C" 

setC :: String -> String -> LocaleString -> LocaleString
setC
    = M.insert

getLang :: String -> LocaleString -> Maybe String
getLang
    = M.lookup

setLang :: String -> String -> LocaleString -> LocaleString
setLang
    = M.insert

newEntry :: FilePath -> DesktopEntry
newEntry
    path = DesktopEntry path $ IniFile [] $ M.fromList [("Desktp Entry", M.empty)]

loadEntry :: FilePath -> IO DesktopEntry
loadEntry path = check =<< decodeIni <$> readFile path
    where
        check (Right x)  = return $ DesktopEntry path x
        check (Left _) = error "Could not load desktop entry file"

saveEntry :: DesktopEntry -> IO ()
saveEntry
    (DesktopEntry path entry) = writeFile path $ encodeIni entry

saveEntryWith :: FilePath -> DesktopEntry -> IO ()
saveEntryWith
    path (DesktopEntry _ entry) = writeFile path $ encodeIni entry

getType :: DesktopEntry -> EntryType
getType
    = typeFromText . fromJust . getValue "Type"

setType :: EntryType -> DesktopEntry -> DesktopEntry
setType
    t = setValue "Type" (show t)
    

getName :: DesktopEntry -> String
getName
    = fromJust . getValue "Name"

setName :: String -> DesktopEntry -> DesktopEntry
setName
    = setValue "Name"

getIcon :: DesktopEntry -> Maybe String
getIcon
    = getValue "Icon"

setIcon :: String -> DesktopEntry -> DesktopEntry
setIcon
    = setValue "Icon"

getGenericName :: DesktopEntry -> Maybe LocaleString
getGenericName
    = getValue "GenericName"

setGenericName :: LocaleString -> DesktopEntry -> DesktopEntry
setGenericName
    = setValue "GenericName"

getPath :: DesktopEntry -> Maybe String
getPath
    = getValue "Path"

setPath :: String -> DesktopEntry -> DesktopEntry
setPath
    = setValue "Path"

getTerminal :: DesktopEntry -> Maybe Bool
getTerminal
    = getValue "Terminal"

setTerminal :: Bool -> DesktopEntry -> DesktopEntry
setTerminal
    = setValue "Terminal"

getExec :: DesktopEntry -> Maybe String
getExec
    = getValue "Exec"

setExec :: String -> DesktopEntry -> DesktopEntry
setExec
    = setValue "Exec"


getUrl :: DesktopEntry -> Maybe String
getUrl
    = getValue "Url"

setUrl :: String -> DesktopEntry -> DesktopEntry
setUrl
    = setValue "Url"

getVersion :: DesktopEntry -> Maybe String
getVersion
    = getValue "Version"

setVersion :: String -> DesktopEntry -> DesktopEntry
setVersion
    = setValue "Version"


execEntry :: DesktopEntry -> IO ProcessHandle
execEntry
    entry = spawnCommand =<< cmd exec
    where
        cmd (x:_) = return x
        cmd []     = error "Invalid command specified as 'Exec' value"
        exec = maybe [] words $ getExec entry

execEntryWith :: [String] -> [String] -> DesktopEntry -> IO ProcessHandle
execEntryWith
    files urls entry = do
    c <- cmd exec
    a <- cargs exec
    spawnProcess c a
    where
        cmd (x:_) = return x
        cmd []    = error "Invalid command specified as 'Exec' value"

        cargs (_:xs) = return $ map re xs
        exec = maybe [] words $ getExec entry

        first [] = []
        first x  = head x

        re "%f" = first files
        re "%F" = unwords files
        re "%u" = first urls
        re "%U" = unwords urls
        re "%i" = maybe [] (\x -> unwords ["--icon", x]) $ getIcon entry
        re x    = x




{-
data DesktopEntry = DesktopEntry
  { deType :: EntryType
  , deVersion :: Maybe Text 
  , deName :: (M.Map Text Text)
  , deGenericName :: Maybe (M.Map Text Text)
  , deNoDisplay :: Maybe Bool
  , deComment :: Maybe (M.Map Text Text)
  , deIcon :: Maybe (M.Map Text Text)
  , deHidden :: Maybe Bool
  , deOnlyShowIn :: Maybe [Text]
  , deNotShowIn :: Maybe [Text]
  , deDBusActivatable :: Maybe Bool
  , deTryExec :: Maybe Text
  , deExec :: Maybe Text
  , dePath :: Maybe Text
  , deTerminal :: Maybe Bool
  , deActions :: Maybe [Text]
  , deMimeType :: Maybe [Text]
  , deCategories :: Maybe [Text]
  , deKeywords :: Maybe (M.Map Text Text)
  , deStartupNotify :: Maybe Bool
  , deStartupWMClass :: Maybe Text
  , deURL :: Maybe Text }
  deriving (Show) -}
