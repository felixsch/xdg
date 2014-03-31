{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module System.Environment.XDG.DesktopEntry
    ( EntryType(..)
    , DesktopEntry
    , loadEntry
    , saveEntry
    , getName
    , getType
    , getEntryValue
    , getExec
    , execEntry
    , execEntryWith
    , getLocalizedEntryValue
    , typeFromText
    , getC )
    where

import Prelude
import Control.Applicative
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import System.Process

import System.Environment.XDG.Internal.Ini

type DesktopEntry = IniFile
type LocaleString = M.Map T.Text T.Text

data EntryType = Application | Directory | Link | Unknown T.Text
    deriving (Show, Eq)

typeFromText :: T.Text -> EntryType
typeFromText "Application" = Application
typeFromText "Directory"   = Directory
typeFromText "Link"        = Link
typeFromText x             = Unknown x


getEntryValue :: (FromValue a) => T.Text -> DesktopEntry -> Maybe a
getEntryValue = getValue "Desktop Entry"

getLocalizedEntryValue :: (FromValue a) => T.Text -> DesktopEntry -> Maybe (M.Map T.Text a)
getLocalizedEntryValue = getValueAll "Desktop Entry"

getC :: LocaleString -> T.Text
getC = fromJust . M.lookup "C" 


loadEntry :: FilePath -> IO DesktopEntry
loadEntry path = check =<< decodeIni <$> TIO.readFile path
    where
        check (Right x)  = return x
        check (Left _) = error "Could not load desktop entry file"

saveEntry :: FilePath -> DesktopEntry -> IO ()
saveEntry path entry = TIO.writeFile path (T.unlines $ encodeIni entry)

getType :: DesktopEntry -> EntryType
getType = typeFromText . fromJust . getEntryValue "Type"

getName :: DesktopEntry -> LocaleString
getName = fromJust . getLocalizedEntryValue "Name"

getIcon :: DesktopEntry -> Maybe T.Text
getIcon = getEntryValue "Icon"

getGenericName :: DesktopEntry -> Maybe LocaleString
getGenericName = getLocalizedEntryValue "GenericName"

getPath :: DesktopEntry -> Maybe T.Text
getPath = getEntryValue "Path"

getTerminal :: DesktopEntry -> Maybe Bool
getTerminal = getEntryValue "Terminal"

getExec :: DesktopEntry -> Maybe T.Text
getExec = getEntryValue "Exec"

getVersion :: DesktopEntry -> Maybe T.Text
getVersion = getEntryValue "Version"


execEntry :: DesktopEntry -> IO ProcessHandle
execEntry entry = spawnCommand =<< T.unpack <$> cmd exec
    where
        cmd (x:_) = return x
        cmd []     = error "Invalid command specified as 'Exec' value"
        exec = maybe [] T.words $ getExec entry

execEntryWith :: [T.Text] -> [T.Text] -> DesktopEntry -> IO ProcessHandle
execEntryWith files urls entry = do
    c <- cmd exec
    a <- cargs exec
    spawnProcess c a
    where
        cmd (x:xs) = return $ T.unpack x
        cmd []    = error "Invalid command specified as 'Exec' value"

        cargs (_:xs) = return $ map T.unpack $ map re xs
        exec = maybe [] T.words $ getExec entry

        first [] = T.empty
        first x  = head x

        re "%f" = first files
        re "%F" = T.unwords files
        re "%u" = first urls
        re "%U" = T.unwords urls
        re "%i" = maybe T.empty (\x -> T.unwords ["--icon", x]) $ getIcon entry
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
