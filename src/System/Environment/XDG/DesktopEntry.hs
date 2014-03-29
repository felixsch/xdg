{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module System.Environment.XDG.DesktopEntry
    ( EntryType(..)
    , DesktopEntry
    , loadEntry
    , getName
    , getType
    , getEntryValue
    , getLocalizedEntryValue
    , typeFromText
    , getC
--    , deLoadFile
--    , deSaveFile )
    )
    where

import Prelude hiding (unlines)
import Control.Applicative
import Data.Maybe
import Data.Text hiding (map, concatMap, concat)
import qualified Data.Text.IO as TIO
import qualified Data.Map as M

import System.Environment.XDG.Internal.Ini

type DesktopEntry = IniFile
type LocaleString = M.Map Text Text

data EntryType = Application | Directory | Link | Unknown Text
    deriving (Show, Eq)

typeFromText :: Text -> EntryType
typeFromText "Application" = Application
typeFromText "Directory"   = Directory
typeFromText "Link"        = Link
typeFromText x             = Unknown x


getEntryValue :: (FromValue a) => Text -> DesktopEntry -> Maybe a
getEntryValue = getValue "Desktop Entry"

getLocalizedEntryValue :: (FromValue a) => Text -> DesktopEntry -> Maybe (M.Map Text a)
getLocalizedEntryValue = getValueAll "Desktop Entry"

getC :: LocaleString -> Text
getC = fromJust . M.lookup "C" 


loadEntry :: FilePath -> IO DesktopEntry
loadEntry path = check =<< decodeIni <$> TIO.readFile path
    where
        check (Right x)  = return x
        check (Left _) = error "Could not load desktop entry file"

saveEntry :: FilePath -> DesktopEntry -> IO ()
saveEntry path entry = TIO.writeFile path (unlines $ encodeIni entry)

getType :: DesktopEntry -> EntryType
getType = typeFromText . fromJust . getEntryValue "Type"

getName :: DesktopEntry -> LocaleString
getName = fromJust . getLocalizedEntryValue "Name"

getGenericName :: DesktopEntry -> Maybe LocaleString
getGenericName = getLocalizedEntryValue "GenericName"

getPath :: DesktopEntry -> Maybe Text
getPath = getEntryValue "Path"

getTerminal :: DesktopEntry -> Maybe Bool
getTerminal = getEntryValue "Terminal"

getExec :: DesktopEntry -> Maybe Text
getExec = getEntryValue "Exec"


getVersion :: DesktopEntry -> Maybe Text
getVersion = getEntryValue "Version"



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
  deriving (Show)


newDesktopEntry :: EntryType -> Text -> DesktopEntry
newDesktopEntry type' name = DesktopEntry
    { deType = type'
    , deName = M.fromList [("C", name)]
    , deVersion = Nothing
    , deGenericName = Nothing
    , deNoDisplay = Nothing
    , deComment = Nothing
    , deIcon = Nothing
    , deHidden = Nothing
    , deOnlyShowIn = Nothing
    , deNotShowIn = Nothing
    , deDBusActivatable = Nothing
    , deTryExec = Nothing
    , deExec = Nothing
    , dePath = Nothing
    , deTerminal = Nothing
    , deActions = Nothing
    , deMimeType = Nothing
    , deCategories = Nothing
    , deKeywords = Nothing
    , deStartupNotify = Nothing
    , deStartupWMClass = Nothing
    , deURL = Nothing
    }

iniToDesktopEntry :: IniFile -> IO DesktopEntry
iniToDesktopEntry i = return $ DesktopEntry
    { deType = typeFromText $ strict "Type" $ get "Type"
    , deVersion = get "Version"
    , deName = strict "Name" $ getAll "Name"
    , deGenericName = getAll "GenericName"
    , deNoDisplay = get "NoDisplay"
    , deComment = getAll "Comment"
    , deIcon = getAll "Icon"
    , deHidden = get "Hidden"
    , deOnlyShowIn = get "OnlyShowIn"
    , deNotShowIn = get "NotShowIn"
    , deDBusActivatable = get "DBusActivatable"
    , deTryExec = get "TryExec"
    , deExec = get "Exec"
    , dePath = get "Path"
    , deTerminal = get "Terminal"
    , deActions = get "Actions"
    , deMimeType = get "MimeType"
    , deCategories = get "Categories"
    , deKeywords = getAll "Keywords"
    , deStartupNotify = get "StartupNotify"
    , deStartupWMClass = get "StartupWMClass"
    , deURL = get "URL" }
    where
      strict _ (Just x) = x
      strict k Nothing  = error $ "Could not find mandatory key: " ++ k 

      get x = getValue "Desktop Entry" x i
      getAll x = getValueAll "Desktop Entry" x i






fromMaybeWith ::  (Text -> a -> [IniPair]) -> Text -> Maybe a -> [IniPair]
fromMaybeWith _ _ Nothing = []
fromMaybeWith f k (Just v) = f k v



mapToList :: (ToValue a) => Text -> M.Map Text a -> [IniPair]
mapToList k = M.foldrWithKey (\i x xs -> (k, (Just i, toValue x)):xs) []




deLoadFile :: FilePath -> IO DesktopEntry
deLoadFile path = iniToDesktopEntry  =<< decodeIni =<< TIO.readFile path

deSaveFile :: FilePath -> DesktopEntry -> IO ()
deSaveFile path d = TIO.writeFile path =<< unlines . encodeIni <$> desktopToIniFile d

-}
