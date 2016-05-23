module Language.Drasil.Misc where

-- Should now make rows.
mkTable :: [a -> b] -> [a] -> [[b]]
mkTable _     []  = []
mkTable []     _  = error "Attempting to make table without data"
mkTable fl (c:cl) = map ($ c) fl : mkTable fl cl
