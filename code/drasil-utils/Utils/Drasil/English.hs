module Utils.Drasil.English (capitalize, stringList) where
    
import Data.Char (toLower, toUpper)

-- | String capitalization
capitalize :: String -> String
capitalize [] = error "capitalize called on an empty String"
capitalize (c:cs) = toUpper c:map toLower cs

-- | Comma separated list with "and" before final item
stringList :: [String] -> String
stringList s = mkStr (filter (not . null) s)
  where mkStr [] = ""
        mkStr [d] = d
        mkStr [d1, d2] = d1 ++ " and " ++ d2
        mkStr (d:ds) = d ++ manyStrs ds
        manyStrs [] = error "impossible case in manyStrs"
        manyStrs [d] = ", and " ++ d
        manyStrs (d:ds) = ", " ++ d ++ manyStrs ds
