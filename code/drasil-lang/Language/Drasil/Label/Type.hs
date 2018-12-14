module Language.Drasil.Label.Type where

-- import reference address from Language.Drasil.References?
data LblType = RefAdd String | MetaLink String | URL String

getAdd :: LblType -> String
getAdd (RefAdd s)   = s
getAdd (MetaLink s) = s
getAdd (URL s)      = s
