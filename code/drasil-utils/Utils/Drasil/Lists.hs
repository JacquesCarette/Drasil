module Utils.Drasil.Lists where

-- | Replaces all elements of a target list that belong to a provided "bad"
--   input list.
replaceAll :: Eq a => [a] -> a -> [a] -> [a]
replaceAll bad repl (c:cs) | c `elem` bad = repl : replaceAll bad repl cs
                           | otherwise    = c : replaceAll bad repl cs
replaceAll _   _    it                    = it

-- | Checks if the first set is a subset of the second.
subsetOf :: Eq a => [a] -> [a] -> Bool
xs `subsetOf` ys = all (`elem` ys) xs
