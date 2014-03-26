{-# LANGUAGE OverloadedStrings #-}

module System.Environment.XDG.DesktopEntry
    ( DesktopEntryType(..)
    , DesktopEntry(..)
    , newDesktopEntry
    , typeFromText
    , deLoadFile
    , deSaveFile )
    where

import Prelude hiding (unlines)
import Control.Applicative
import Data.Text hiding (map, concatMap, concat)
import qualified Data.Text.IO as TIO
import qualified Data.Map as M

import System.Environment.XDG.Parser.Ini

data DesktopEntryType = Application | Directory | Link | Unknown Text
    deriving (Show, Eq)

typeFromText :: Text -> DesktopEntryType
typeFromText "Application" = Application
typeFromText "Directory"   = Directory
typeFromText "Link"        = Link
typeFromText x             = Unknown x

instance ToValue DesktopEntryType where
    toValue = IString . pack . show


data DesktopEntry = DesktopEntry
  { deType :: DesktopEntryType
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


newDesktopEntry :: DesktopEntryType -> Text -> DesktopEntry
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



desktopToIniFile :: DesktopEntry -> IO IniFile
desktopToIniFile de = return [("Desktop Entry", concat values)]
    where
        values = 
            [ type'
            , set "Version" deVersion
            , name
            , setAll "GenericName" deGenericName
            , set "NoDisplay" deNoDisplay
            , setAll "Comment" deComment
            , setAll "Icon" deIcon
            , set "Hidden" deHidden
            , set "OnlyShowIn" deOnlyShowIn
            , set "NotShowIn" deNotShowIn
            , set "DBusActivatable" deDBusActivatable
            , set "TryExec" deTryExec
            , set "Exec" deExec
            , set "Path" dePath
            , set "Terminal" deTerminal
            , set "Actions" deActions
            , set "MimeType" deMimeType
            , set "Categories" deCategories
            , setAll "Keywords" deKeywords
            , set "StartupNotify" deStartupNotify
            , set "StartupWMClass" deStartupWMClass
            , set "URL" deURL
            ]
        type'  = [("Type", (Nothing, toValue $ deType de))] 
        name   = mapToList "Name" $ deName de
        set k f = fromMaybeWith (\k v -> [(k, (Nothing, toValue v))] ) k $ f de
        setAll k f = fromMaybeWith mapToList k $ f de

deLoadFile :: FilePath -> IO DesktopEntry
deLoadFile path = iniToDesktopEntry  =<< decodeIni =<< TIO.readFile path

deSaveFile :: FilePath -> DesktopEntry -> IO ()
deSaveFile path d = TIO.writeFile path =<< unlines . encodeIni <$> desktopToIniFile d
