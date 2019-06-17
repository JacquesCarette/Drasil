module Utils.Drasil.English (capitalize) where
    
import Data.Char (toLower, toUpper)

-- | String capitalization
capitalize :: String -> String
capitalize [] = error "capitalize called on an empty String"
capitalize (c:cs) = toUpper c:map toLower cs
