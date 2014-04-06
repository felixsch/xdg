{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module System.Environment.XDG.DesktopEntry
    ( EntryType(..)
    , DesktopEntry
    , LocaleString
    , loadEntry
    , saveEntry
    , getName
    , getType
    , getTerminal
    , getIcon
    , getExec
    , execEntry
    , execEntryWith
    , getValue
    , typeFromText
    , getC )
    where

import Prelude
import Control.Applicative
import Data.Maybe
import qualified Data.Map as M
import System.Process

import System.Environment.XDG.Internal.Ini

type DesktopEntry = IniFile
type LocaleString = M.Map String String

data EntryType = Application | Directory | Link | Unknown String
    deriving (Show, Eq)

typeFromText :: String -> EntryType
typeFromText "Application" = Application
typeFromText "Directory"   = Directory
typeFromText "Link"        = Link
typeFromText x             = Unknown x


getValue :: (CastValue a) => String -> DesktopEntry -> Maybe a
getValue = getKey "Desktop Entry"

getC :: LocaleString -> String
getC = fromJust . M.lookup "C" 


loadEntry :: FilePath -> IO DesktopEntry
loadEntry path = check =<< decodeIni <$> readFile path
    where
        check (Right x)  = return x
        check (Left _) = error "Could not load desktop entry file"

saveEntry :: FilePath -> DesktopEntry -> IO ()
saveEntry path entry = writeFile path $ encodeIni entry

getType :: DesktopEntry -> EntryType
getType = typeFromText . fromJust . getValue "Type"

getName :: DesktopEntry -> String
getName = fromJust . getValue "Name"

getIcon :: DesktopEntry -> Maybe String
getIcon = getValue "Icon"

getGenericName :: DesktopEntry -> Maybe LocaleString
getGenericName = getValue "GenericName"

getPath :: DesktopEntry -> Maybe String
getPath = getValue "Path"

getTerminal :: DesktopEntry -> Maybe Bool
getTerminal = getValue "Terminal"

getExec :: DesktopEntry -> Maybe String
getExec = getValue "Exec"

getVersion :: DesktopEntry -> Maybe String
getVersion = getValue "Version"


execEntry :: DesktopEntry -> IO ProcessHandle
execEntry entry = spawnCommand =<< cmd exec
    where
        cmd (x:_) = return x
        cmd []     = error "Invalid command specified as 'Exec' value"
        exec = maybe [] words $ getExec entry

execEntryWith :: [String] -> [String] -> DesktopEntry -> IO ProcessHandle
execEntryWith files urls entry = do
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
