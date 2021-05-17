module StringUtils where

{- The below functions are based on that of MissingH's Data.String.Utils
https://hackage.haskell.org/package/MissingH-1.4.3.0/docs/Data-String-Utils.html
-}

strip :: String -> String
strip = rstrip . lstrip

lstrip :: String -> String
lstrip it@(c:cs) | c `elem` " \t\r\n" = lstrip cs
                 | otherwise          = it
lstrip [] = ""

rstrip :: String -> String
rstrip = reverse . lstrip . reverse
