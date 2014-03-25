module Internal.IniDecode 
    ( decodeIni )
    where



import Data.Text hiding (takeWhile)
import Data.Attoparsec
import Control.Applicative

import IniTypes


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

