-- | A collection of 'String'-handling routines as well as one for making tables.
module Language.Drasil.Misc (mkTable, repUnd) where

{- |
  Create a table body (not including header row) by applying the given
  functions to the column elements of the table rows (in order).
  The first argument is a list of functions to be applied (one per column).
  This essentially creates the rows.
  The second argument is a list of elements apply the functions to.

  For example, @mkTable [id, *5] [1,2,3]@ should produce a table:
  
  > | 1 |  5 |
  > | 2 | 10 |
  > | 3 | 15 |
  
-}
mkTable :: [a -> b] -> [a] -> [[b]]
mkTable fs = map (\x -> map ($ x) fs)

-- | Replace underscores in a string with periods (@.@).
repUnd :: String -> String
repUnd = map rep
  where
    rep :: Char -> Char
    rep '_' = '.'
    rep c = c
