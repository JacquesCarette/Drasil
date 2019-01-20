module Language.Drasil.ShortName(ShortName, getStringSN, shortname') where

data ShortName = ShortNm String

getStringSN :: ShortName -> String
getStringSN (ShortNm s) = s

shortname' :: String -> ShortName
shortname' = ShortNm
