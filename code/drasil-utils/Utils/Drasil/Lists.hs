module Utils.Drasil.Lists where

replaceAll :: Eq a => [a] -> a -> [a] -> [a]
replaceAll bad repl (c:cs) | c `elem` bad = repl : replaceAll bad repl cs
                           | otherwise    = c : replaceAll bad repl cs
replaceAll _   _    it                    = it
