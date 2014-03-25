{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Internal.IniTypes
    ( IniValue(..)
    , FromValue(..)
    , ToValue(..)
    , encodeIni
    , decodeIni
    , getValue
    , getValueAll) where


import Prelude hiding (concat, takeWhile)
import Control.Applicative
import Data.Text hiding (takeWhile, foldl,  map, concatMap)
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Data.Attoparsec.Text




data IniValue = IBool Bool
              | IString Text
              | IArray [Text]
              | IDouble Double
              | IComment Text
              deriving (Show, Eq)

class FromValue a where
    from :: IniValue -> a

instance FromValue Text where
    from (IString x) = x
    from (IDouble x) = pack $ show x
    from (IBool x)
     | x == True       = "true"
     | otherwise       = "false"

instance FromValue [Text] where
    from (IArray x) = x
    from x         = [from x]

instance FromValue Int where
    from (IDouble x) = floor x

instance FromValue Bool where
    from (IBool x) = x


class ToValue a where
    to :: a -> IniValue

instance ToValue Text where
    to x = IString x

instance ToValue [Text] where
    to x = IArray x

instance ToValue Int where
    to x = IDouble $ fromIntegral x

instance ToValue Double where
    to x = IDouble x

instance ToValue Bool where
    to x = IBool x

type IniPair = (Text, (Maybe Text, IniValue))
type IniSection = (Text, [IniPair])
type IniFile  = [IniSection]


pBool :: Parser Bool 
pBool = string "true"  *> return True
    <|> string "false" *> return False

pString :: Parser Text
pString = takeWhile $ notInClass ";\n"

pArray :: Parser [Text]
pArray = many1 ( pString <* char ';')


pValue :: Parser IniValue
pValue = IBool <$> pBool 
    <|> IDouble <$> double
    <|> IArray <$> pArray
    <|> IString <$> pString


pLine :: Parser IniPair
pLine = liftA2 (,) key value
    where
        delim c = c /= '=' && c /= '['
        key = pack <$> (many1 $ satisfy delim)
        index = choice [  Just . pack <$> (char '[' *> (many1 $ notChar ']') <* char ']')
                       , return Nothing ]
        value = liftA2 (,) index (char '=' *> pValue)

pComment :: Parser Text
pComment = pack <$> (char '#' *> many anyChar)

pSection :: Parser Text
pSection = pack <$> (char '[' *> (many $ notChar ']') <* char ']')


pIni :: Parser IniFile
pIni = many $ skipSpace *> sections
    where
        sections = liftA2 (,) (pSection <* endOfLine) (many (pLine <* (takeWhile $ inClass " \t\r\n")))



decodeIni :: Text -> IO IniFile
decodeIni c = case parseOnly pIni c of 
                Right x -> return x
                Left x  -> error $ "Could not parse IniFile: " ++ x 


encodeValue :: IniValue -> Text
encodeValue (IDouble x) = pack $ show x
encodeValue (IString x) = x
encodeValue (IBool True) = "true"
encodeValue (IBool False) = "false"
encodeValue (IArray x) = foldl (\x y -> append x $ snoc y ';') "" x
encodeValue (IComment x) = cons '#' x


encodePairs :: [IniPair] -> [Text]
encodePairs pairs = map (\x -> concat [key x, index x, value x]) pairs
    where
        maybeIndex Nothing = "="
        maybeIndex (Just x)  = concat ["[", x, "]="]
        key   = fst
        index = maybeIndex . fst . snd
        value = encodeValue . snd . snd



encodeIni :: IniFile -> [Text]
encodeIni ini = concatMap (\x -> [section x] ++ (encodePairs $ snd x)) ini
    where
        section x = concat ["[", fst x, "]"]



lookupAll :: (Eq a) => a -> [(a,b)] -> [b]
lookupAll key [] = []
lookupAll key ((x,y):z)
    | key == x   = [y] ++ lookupAll key z
    | otherwise  = lookupAll key z


sectionWith :: IniFile -> Text -> ([IniPair] -> Maybe a) -> Maybe a
sectionWith ini sec func = case lookup sec ini of
                            Just x -> func x
                            Nothing -> Nothing

getAll :: (FromValue a) => Text -> Text -> IniFile -> Maybe a
getAll sec k ini = sectionWith ini sec (\x -> case lookup k x of
                                                Just x -> Just $ from $ snd x
                                                Nothing -> Nothing)


getValueAll :: (FromValue a) => Text -> Text -> IniFile -> Maybe (M.Map Text a)
getValueAll sec k ini = sectionWith ini sec (\x -> mkMaybe $ lookupAll k x)
    where   
        mkMaybe [] = Nothing
        mkMaybe x  = Just (M.fromList $ genList x)

        genList (((Just x), val):xs) = [(x, from val)] ++ genList xs
        genList ((Nothing, val):xs) = [("C", from val)] ++ genList xs
        genList [] = []
