module Language.Drasil.RefHelpers where

import Data.Char (isAlphaNum)

-- | replace underscore in strings with "."
repUnd :: String -> String
repUnd s = map (\c -> if c == '_' then '.' else c) s
