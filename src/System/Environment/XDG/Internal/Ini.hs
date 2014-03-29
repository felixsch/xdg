{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Environment.XDG.Internal.Ini
    ( IniValue(..)
    , IniFile
    , IniPair
    , FromValue(..)
    , ToValue(..)
    , encodeIni
    , decodeIni
    , getValue
    , getValueAll) where


import Prelude hiding (concat, takeWhile)
import Control.Applicative
import Data.Text hiding (takeWhile, foldl,  map, concatMap)
import Data.Either
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Data.Attoparsec.Text


data IniValue = IBool Bool
              | IString Text
              | IArray [Text]
              | IDouble Double
              deriving (Show, Eq)

class FromValue a where
    fromValue :: IniValue -> a

instance FromValue Text where
    fromValue (IString x) = x
    fromValue (IDouble x) = pack $ show x
    fromValue (IBool x)
     | x               = "true"
     | otherwise       = "false"

instance FromValue [Text] where
    fromValue (IArray x) = x
    fromValue  x         = [fromValue x]

instance FromValue Int where
    fromValue (IDouble x) = floor x

instance FromValue Bool where
    fromValue (IBool x) = x

class ToValue a where
    toValue :: a -> IniValue

instance ToValue Text where
    toValue = IString

instance ToValue [Text] where
    toValue = IArray

instance ToValue Int where
    toValue = IDouble . fromIntegral

instance ToValue Double where
    toValue = IDouble

instance ToValue Bool where
    toValue = IBool

data IniComment = IniComment Text
    deriving (Show)
type IniPair = (Text, (Maybe Text, IniValue))
type IniSection = (Text, [Either IniPair IniComment])
type IniFile  = [IniSection]

pBool :: Parser Bool 
pBool = string "true"  *> return True
    <|> string "false" *> return False

pString :: Parser Text
pString = takeWhile $ notInClass ";\n"

pArray :: Parser [Text]
pArray = many1 ( pString <* char ';')


pComment :: Parser (Either IniPair IniComment)
pComment = (Right . IniComment . pack) <$> (char '#' *> many anyChar)


pValue :: Parser IniValue
pValue = IBool <$> pBool 
    <|> IDouble <$> double
    <|> IArray <$> pArray
    <|> IString <$> pString


pLine :: Parser (Either IniPair IniComment)
pLine = Left <$> liftA2 (,) key value
    where
        delim c = c /= '=' && c /= '['
        key = pack <$> (many1 $ satisfy delim)
        lang = choice [  Just . pack <$> (char '[' *> (many1 $ notChar ']') <* char ']')
                       , return Nothing ]
        value = liftA2 (,) lang (char '=' *> pValue)


pSection :: Parser Text
pSection = pack <$> (char '[' *> (many $ notChar ']') <* char ']')


pIni :: Parser IniFile
pIni = many $ skipSpace *> sections
    where
        sections = liftA2 (,) (pSection <* endOfLine) (many (pComment <|> pLine <* (takeWhile $ inClass " \t\r\n")))


decodeIni :: Text -> Either String IniFile
decodeIni = parseOnly pIni


encodeValue :: IniValue -> Text
encodeValue (IDouble x) = pack $ show x
encodeValue (IString x) = x
encodeValue (IBool True) = "true"
encodeValue (IBool False) = "false"
encodeValue (IArray arr) = foldl (\x y -> append x $ snoc y ';') "" arr


encodePairs :: [Either IniPair IniComment] -> [Text]
encodePairs = map line
    where
        maybeLang Nothing = "="
        maybeLang (Just "C") = "="
        maybeLang (Just x)  = concat ["[", x, "]="]
        line (Left x) = concat [key x, lang x, value x]
        line (Right (IniComment x)) = concat ["#", x]
        key   = fst
        lang = maybeLang . fst . snd
        value = encodeValue . snd . snd



encodeIni :: IniFile -> [Text]
encodeIni = concatMap (\x -> (section $ fst x) : (encodePairs $ snd x))
    where
        section x = concat ["[", x, "]"]



lookupAll :: (Eq a) => a -> [(a,b)] -> [b]
lookupAll _ [] = []
lookupAll key ((x,y):z)
    | key == x   = y : lookupAll key z
    | otherwise  = lookupAll key z


sectionWith :: IniFile -> Text -> ([Either IniPair IniComment] -> Maybe a) -> Maybe a
sectionWith ini sec func = case lookup sec ini of
                            Just x -> func x
                            Nothing -> Nothing

getValue :: (FromValue a) => Text -> Text -> IniFile -> Maybe a
getValue sec k ini = sectionWith ini sec (\x -> case lookup k (lefts x) of
                                                Just f -> Just $ fromValue $ snd f
                                                Nothing -> Nothing)


getValueAll :: (FromValue a) => Text -> Text -> IniFile -> Maybe (M.Map Text a)
getValueAll sec k ini = sectionWith ini sec (\x -> mkMaybe $ lookupAll k $ lefts x)
    where   
        mkMaybe [] = Nothing
        mkMaybe x  = Just $ M.fromList $ genList x

        genList ((Just x, val):xs) = (x, fromValue val) : genList xs
        genList ((Nothing, val):xs) = ("C", fromValue val) : genList xs
        genList [] = []
