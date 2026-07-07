-- Contains a function that removes special characters from strings.
module Data.String.Extras (
  toPlainName
) where

import Data.List.Extras (replaceAll)

-- | Replace occurences of special characters (@",~`-=!@#$%^&*+[]\\;'/|\"<>? "@)
--   with underscores (@"_"@).
--
--   TODO: This can probably become a bit more comprehensive, anything other
--   than a-z, A-Z, or 0-9 could probably be replaced.
toPlainName :: String -> String
toPlainName = replaceAll ",~`-=!@#$%^&*+[]\\;'/|\"<>? " '_'
