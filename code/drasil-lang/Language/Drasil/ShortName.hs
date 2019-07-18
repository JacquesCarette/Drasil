module Language.Drasil.ShortName where

newtype ShortName = ShortNm String

getStringSN :: ShortName -> String
getStringSN (ShortNm s) = s

shortname' :: String -> ShortName
shortname' = ShortNm
