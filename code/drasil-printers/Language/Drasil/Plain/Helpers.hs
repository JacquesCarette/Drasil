module Language.Drasil.Plain.Helpers where

toPlainName :: String -> String
toPlainName (c:cs) | c `elem` ",~`-=!@#$%^&*+[]\\;'/|\"<>? " = '_' : toPlainName cs
                   | otherwise                               = c : toPlainName cs
toPlainName cs = cs
