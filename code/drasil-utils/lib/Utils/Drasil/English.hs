-- | Useful functions for working with English-related 'String's.
module Utils.Drasil.English (capitalize, stringList) where

import Data.Char (toLower, toUpper)

-- | String capitalization.
capitalize :: String -> String
capitalize [] = error "capitalize called on an empty String"
capitalize (c:cs) = toUpper c:map toLower cs

-- | Comma separated list with "and" before final item.
stringList :: [String] -> String
stringList s = mkStr (filter (not . null) s)
  where
    mkStr :: [String] -> String
    mkStr []       = ""
    mkStr [d]      = d
    mkStr [d1, d2] = d1 ++ " and " ++ d2 -- TODO: When you have a list of >=3 items, your last 2 should still have a comma between them.
    mkStr (d:ds)   = d ++ manyStrs ds

    manyStrs :: [String] -> String
    manyStrs []     = error "impossible case in manyStrs" -- TODO: Make explicit why this is an impossible case
    manyStrs [d]    = ", and " ++ d
    manyStrs (d:ds) = ", " ++ d ++ manyStrs ds
