module StringUtils where

strip :: String -> String
strip = rstrip . lstrip

lstrip :: String -> String
lstrip (c:cs) | c `elem` " \t\r\n" = lstrip cs
              | otherwise          = cs
lstrip [] = ""

rstrip :: String -> String
rstrip = reverse . lstrip . reverse
