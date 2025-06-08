-- Contains a function that removes special characters from a string.
module Utils.Drasil.Strings where

import Utils.Drasil.Lists (replaceAll)

-- | Replace occurences of special characters (@",~`-=!@#$%^&*+[]\\;'/|\"<>? "@)
--   with underscores (@"_"@).
-- 
--   TODO: This can probably become a bit more comprehensive, anything other
--   than a-z, A-Z, or 0-9 could probably be replaced.
toPlainName :: String -> String
toPlainName = replaceAll ",~`-=!@#$%^&*+[]\\;'/|\"<>? " '_'

-- | Replace underscores in a string with periods (@.@).
repUnd :: String -> String
repUnd = map rep
  where
    rep :: Char -> Char
    rep '_' = '.'
    rep c = c
