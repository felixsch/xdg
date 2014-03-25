{-# LANGUAGE OverloadedStrings #-}


module Internal.IniEncode
    ( encodeValue
    , encodePairs
    , encodeIni )
    where

import Prelude hiding (concat)
import Data.Text hiding (concatMap, map)
import Control.Applicative

import IniTypes

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
encodeIni ini = concatMap (\x -> [section x] ++ (encodeValues $ snd x)) ini
    where
        section x = concat ["[", fst x, "]"]
