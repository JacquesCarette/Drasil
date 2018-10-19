module Language.Drasil.Label.Type where

-- import reference address from Language.Drasil.References?
data LblType = RefAdd String | MetaLink String | URI String

getAdd :: LblType -> String
getAdd (RefAdd s)   = s
getAdd (MetaLink s) = s
getAdd (URI s)      = s
