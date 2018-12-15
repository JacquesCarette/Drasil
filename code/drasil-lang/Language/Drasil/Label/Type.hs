module Language.Drasil.Label.Type where

data LblType = RefAdd String | MetaLink String | URL String

getAdd :: LblType -> String
getAdd (RefAdd s)   = s
getAdd (MetaLink s) = s
getAdd (URL s)      = s
