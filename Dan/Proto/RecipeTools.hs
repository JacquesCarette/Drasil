module RecipeTools where --Recipe or layout tools? Or something else?

-- Should now make rows, then columns.
mkTable :: [a -> b] -> [a] -> [[b]]
mkTable []     _  = []
mkTable fl (c:cl) = map ($ c) fl : mkTable fl cl