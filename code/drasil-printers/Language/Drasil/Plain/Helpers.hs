module Language.Drasil.Plain.Helpers where

-- | Replace occurences of special characters (",~`-=!@#$%^&*+[]\\;'/|\"<>? ") with underscores ("_").
toPlainName :: String -> String
toPlainName (c:cs) | c `elem` ",~`-=!@#$%^&*+[]\\;'/|\"<>? " = '_' : toPlainName cs
                   | otherwise                               = c : toPlainName cs
toPlainName cs = cs
