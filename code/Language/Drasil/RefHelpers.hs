module Language.Drasil.RefHelpers where

import Language.Drasil.Spec

import Data.Char (isAlphaNum)

-- | helper to get first 4 letters of each word. Used by inferName
firstFourLetters :: String -> String
firstFourLetters s1 = (filter (\x -> not (x `elem` ",.?!")) stringId)
  where stringId = concat (map (take 4) (words s1))


-- | replace underscore in strings with "."
repUnd :: String -> String
repUnd s = map (\c -> if c == '_' then '.' else c) s

-- | helper for filtering out any non-alpha-numeric characters from a string
alphanumOnly :: String -> String
alphanumOnly = filter (isAlphaNum)
