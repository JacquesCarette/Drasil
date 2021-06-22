module Utils.Drasil.Lists where

replaceAll :: Eq a => [a] -> a -> [a] -> [a]
replaceAll badElems repl it@(c:cs)
  | c `elem` badElems = repl : replaceAll badElems repl cs
  | otherwise         = it
replaceAll _        _    it = it
