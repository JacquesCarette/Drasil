module Language.Drasil.Utils where

noSpaces :: String -> String
noSpaces s
  | (' ' `elem` s) == False = s
  | otherwise               = error "String has at least one space in it."
