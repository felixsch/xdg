{-# LANGUAGE OverloadedStrings #-}

module System.Environment.XDG.DesktopFile
    ( DesktopFileType(..)
    , DesktopFile(..)
    , newDesktopFile
    , typeFromText
    , dfLoadFile
    , dfSaveFile )
    where

import Prelude hiding (unlines)
import Control.Applicative
import Data.Text hiding (map, concatMap, concat)
import qualified Data.Text.IO as TIO
import qualified Data.Map as M

import System.Environment.XDG.Parser.Ini

data DesktopFileType = Application | Directory | Link | Unknown Text
    deriving (Show, Eq)

typeFromText :: Text -> DesktopFileType
typeFromText "Application" = Application
typeFromText "Directory"   = Directory
typeFromText "Link"        = Link
typeFromText x             = Unknown x

instance ToValue DesktopFileType where
    toValue = IString . pack . show


data DesktopFile = DesktopFile
  { dfType :: DesktopFileType
  , dfVersion :: Maybe Text 
  , dfName :: (M.Map Text Text)
  , dfGenericName :: Maybe (M.Map Text Text)
  , dfNoDisplay :: Maybe Bool
  , dfComment :: Maybe (M.Map Text Text)
  , dfIcon :: Maybe (M.Map Text Text)
  , dfHidden :: Maybe Bool
  , dfOnlyShowIn :: Maybe [Text]
  , dfNotShowIn :: Maybe [Text]
  , dfDBusActivatable :: Maybe Bool
  , dfTryExec :: Maybe Text
  , dfExec :: Maybe Text
  , dfPath :: Maybe Text
  , dfTerminal :: Maybe Bool
  , dfActions :: Maybe [Text]
  , dfMimeType :: Maybe [Text]
  , dfCategories :: Maybe [Text]
  , dfKeywords :: Maybe (M.Map Text Text)
  , dfStartupNotify :: Maybe Bool
  , dfStartupWMClass :: Maybe Text
  , dfURL :: Maybe Text }
  deriving (Show)


newDesktopFile :: DesktopFileType -> Text -> DesktopFile
newDesktopFile type' name = DesktopFile 
    { dfType = type'
    , dfName = M.fromList [("C", name)]
    , dfVersion = Nothing
    , dfGenericName = Nothing
    , dfNoDisplay = Nothing
    , dfComment = Nothing
    , dfIcon = Nothing
    , dfHidden = Nothing
    , dfOnlyShowIn = Nothing
    , dfNotShowIn = Nothing
    , dfDBusActivatable = Nothing
    , dfTryExec = Nothing
    , dfExec = Nothing
    , dfPath = Nothing
    , dfTerminal = Nothing
    , dfActions = Nothing
    , dfMimeType = Nothing
    , dfCategories = Nothing
    , dfKeywords = Nothing
    , dfStartupNotify = Nothing
    , dfStartupWMClass = Nothing
    , dfURL = Nothing
    }

iniToDesktopFile :: IniFile -> IO DesktopFile
iniToDesktopFile i = return $ DesktopFile
    { dfType = typeFromText $ strict "Type" $ get "Type"
    , dfVersion = get "Version"
    , dfName = strict "Name" $ getAll "Name"
    , dfGenericName = getAll "GenericName"
    , dfNoDisplay = get "NoDisplay"
    , dfComment = getAll "Comment"
    , dfIcon = getAll "Icon"
    , dfHidden = get "Hidden"
    , dfOnlyShowIn = get "OnlyShowIn"
    , dfNotShowIn = get "NotShowIn"
    , dfDBusActivatable = get "DBusActivatable"
    , dfTryExec = get "TryExec"
    , dfExec = get "Exec"
    , dfPath = get "Path"
    , dfTerminal = get "Terminal"
    , dfActions = get "Actions"
    , dfMimeType = get "MimeType"
    , dfCategories = get "Categories"
    , dfKeywords = getAll "Keywords"
    , dfStartupNotify = get "StartupNotify"
    , dfStartupWMClass = get "StartupWMClass"
    , dfURL = get "URL" }
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



desktopToIniFile :: DesktopFile -> IO IniFile
desktopToIniFile df = return [("Desktop Entry", concat values)]
    where
        values = 
            [ type'
            , set "Version" dfVersion
            , name
            , setAll "GenericName" dfGenericName
            , set "NoDisplay" dfNoDisplay
            , setAll "Comment" dfComment
            , setAll "Icon" dfIcon
            , set "Hidden" dfHidden
            , set "OnlyShowIn" dfOnlyShowIn
            , set "NotShowIn" dfNotShowIn
            , set "DBusActivatable" dfDBusActivatable
            , set "TryExec" dfTryExec
            , set "Exec" dfExec
            , set "Path" dfPath
            , set "Terminal" dfTerminal
            , set "Actions" dfActions
            , set "MimeType" dfMimeType
            , set "Categories" dfCategories
            , setAll "Keywords" dfKeywords
            , set "StartupNotify" dfStartupNotify
            , set "StartupWMClass" dfStartupWMClass
            , set "URL" dfURL
            ]
        type'  = [("Type", (Nothing, toValue $ dfType df))] 
        name   = mapToList "Name" $ dfName df
        set k f = fromMaybeWith (\k v -> [(k, (Nothing, toValue v))] ) k $ f df
        setAll k f = fromMaybeWith mapToList k $ f df

dfLoadFile :: FilePath -> IO DesktopFile
dfLoadFile path = iniToDesktopFile  =<< decodeIni =<< TIO.readFile path

dfSaveFile :: FilePath -> DesktopFile -> IO ()
dfSaveFile path d = TIO.writeFile path =<< unlines . encodeIni <$> desktopToIniFile d
