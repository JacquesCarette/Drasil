{-# OPTIONS -Wall #-} 
module RecipeTools where --Recipe or layout tools? Or something else?

-- Should now make rows.
mkTable :: [a -> b] -> [a] -> [[b]]
mkTable _     []  = []
mkTable []     _  = error "Attempting to make table without data"
mkTable fl (c:cl) = map ($ c) fl : mkTable fl cl