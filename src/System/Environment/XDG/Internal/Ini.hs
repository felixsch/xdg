{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module System.Environment.XDG.Internal.Ini
    ( IniValue(..)
    , IniFile(..)
    , IniSection
    , CastValue(..)
    , encodeIni
    , decodeIni
    , getHeader
    , setHeader
    , getKey
    , setKey) where

import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)
import Control.Monad()
import Control.Monad.IO.Class()
import qualified Data.Map as M
import Numeric (readFloat)

data IniValue = IBool Bool
              | IString String
              | INumber Double
              | IArray [String]
              | IMap (M.Map String String)
              deriving(Show, Eq)


type IniSection = M.Map String IniValue
data IniFile = IniFile [String] (M.Map String IniSection)
    deriving (Show)

class CastValue a where
    from :: IniValue -> Maybe a
    to   :: a -> IniValue

instance CastValue String where
    from (IString x)    = Just x
    from (IMap x)       = Just $ fromMaybe [] $ M.lookup "C" x
    from (IArray [])    = Just []
    from (IBool True)   = Just "true"
    from (IBool False)  = Just "false"
    from (IArray (x:_)) = Just x
    from _              = Nothing
    to                  = IString

instance CastValue [String] where
    from (IArray x)    = Just x
    from (IMap x)      = Just $ M.elems x
    from _             = Nothing
    to                 = IArray

instance CastValue (M.Map String String) where
    from (IMap x)      = Just x
    from (IString x)    = Just $ M.fromList [("C", x)]
    from _             = Nothing
    to                 = IMap

instance CastValue Bool where
    from (IBool x)     = Just x
    from _             = Nothing
    to                 = IBool

instance CastValue Int where
    from (INumber x)   = Just $ floor x
    from _             = Nothing
    to                 = INumber . fromIntegral

instance CastValue Double where
    from (INumber x)   = Just x
    to                 = INumber

lexme:: forall a. Parser a -> Parser a
lexme p
    = p <* (whiteSpaces <|> comment <|> newl)
    where
        whiteSpaces = many $ oneOf " \t\r"
        comment = between (char '#') (char '\n') $ many $ noneOf "\n"
        newl = lexme (string "\n")

pBool :: Parser Bool
pBool 
    =   string "true"  *> return True
    <|> string "false" *> return False

pNumber :: Parser Double
pNumber = do
  s <- getInput
  case readFloat s of
    [(n,s')]  -> n <$ setInput s'
    _         -> empty


pString :: Parser String
pString 
    = (lexme $ many $ special <|> noneOf ";\n")
    where
        special = string "\\;"  *> return ';'
              <|> string "\\n"  *> return '\n'
              <|> string "\\t"  *> return '\t'
              <|> string "\\r"  *> return '\r'
              <|> string "\\\\" *> return '\\'

pArray :: Parser [String]
pArray 
    = many1 (pString <* char ';')


pIndex :: Parser String
pIndex
    = between (string "[") (string "]") (many $ noneOf "]")

pName :: Parser String
pName
    = many (oneOf " \t\r") *> (lexme $ many (noneOf "=[] "))


pValue :: Parser IniValue
pValue
    =   INumber <$> pNumber
    <|> try (IArray <$> pArray)
    <|> try (IBool <$> pBool)
    <|> IString <$> pString


pEquals :: Parser Char
pEquals = lexme $ char '='

pEnd :: Parser ()
pEnd = char '\n' *> return () <|> eof

pNormal :: Parser (String, IniValue)
pNormal 
    = (,) <$> pName <* pEquals <*> pValue <* pEnd

pMap :: Parser (String, IniValue)
pMap = do 
        name <- pName
        pEquals
        value <- pString
        pEnd
        tr <- (many1 $ translations name)
        return (name, cob value tr)

    where
        cob v m = IMap $ M.fromList $ ("C", v) : m
        translations name = (,) <$ (lexme $ string name) <*> pIndex <* pEquals <*> pString <* pEnd


pSection :: Parser (String, IniSection)
pSection
    = (,) <$> name <*> values
    where
        name = lexme (between (char '[') (char ']') $ many $ noneOf "]") <* char '\n'
        values = M.fromList <$> many value
        value  = try pMap <|> try pNormal

pIni :: Parser IniFile
pIni
    = IniFile <$ skip <*> header <*> sections
    where
        header   = many $ between (char '#') (char '\n') $ many (noneOf "\n")
        sections = M.fromList <$> (many $ skip *> pSection <* skip)
        skip     = many $ oneOf " \r\t\n"

decodeIni :: String -> Either String IniFile
decodeIni
    c = case parse pIni "[Ini]" c of
          Left err -> Left $ show err
          Right x  -> Right x

encodeValue :: String -> IniValue -> String
encodeValue x (IBool b)   = x ++ "=" ++ show b
encodeValue x (INumber n) = x ++ "=" ++ show n
encodeValue x (IString s) = x ++ "=" ++ s
encodeValue x (IArray a)  = x ++ "=" ++ foldl1 (\s e -> s ++ e ++ ";") a
encodeValue x (IMap m )   = M.foldlWithKey (genMap x) [] m 
    where 
        genMap n l "C" v   = l ++ n ++ "=" ++ v ++ "\n"
        genMap n l i   v   = l ++ n ++ "[" ++ i ++ "]" ++ "=" ++ v ++ "\n"

encodeIni :: IniFile -> String
encodeIni
    (IniFile c ini) = unlines $ (map ("# " ++) c) ++ (map (uncurry gen) $ M.toList ini)
    where
        gen x vs = "[" ++ x ++ "]\n" ++ M.foldlWithKey (\a k v -> a ++ encodeValue k v ++ "\n") [] vs


getHeader :: IniFile -> [String]
getHeader
    (IniFile c _) = c

setHeader :: [String] -> IniFile -> IniFile
setHeader
    c (IniFile _ ini) = IniFile c ini


getKey :: (CastValue a) => String -> String -> IniFile -> Maybe a
getKey
    sec key (IniFile _ ini) = maybe Nothing (\x -> from =<< M.lookup key x) $ M.lookup sec ini

setKey :: (CastValue a) => String -> String -> a -> IniFile -> IniFile
setKey
    sec key val (IniFile co ini) =IniFile co $ M.insert sec set' ini
    where
        set' = maybe (M.fromList [(key, to val)]) (M.insert key (to val)) $ M.lookup sec ini
