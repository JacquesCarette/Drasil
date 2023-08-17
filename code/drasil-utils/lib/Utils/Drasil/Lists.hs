-- | Functions for working with lists.
module Utils.Drasil.Lists where

import Data.List

-- | Check if list has at least 2 elements.
atLeast2 :: [a] -> Bool
atLeast2 (_:_:_) = True
atLeast2 _       = False

-- | Replaces all elements of a target list that belong to a provided "bad"
--   input list.
replaceAll :: Eq a => [a] -> a -> [a] -> [a]
replaceAll bad repl (c:cs) | c `elem` bad = repl : replaceAll bad repl cs
                           | otherwise    = c : replaceAll bad repl cs
replaceAll _   _    it                    = it

-- | Checks if the first set is a subset of the second.
subsetOf :: Eq a => [a] -> [a] -> Bool
xs `subsetOf` ys = all (`elem` ys) xs

-- | Sort a list, removing all duplicates
nubSort :: Ord a => [a] -> [a]
nubSort = nub . sort

-- | Interweaves two lists together @[[a,b,c],[d,e,f]] -> [a,d,b,e,c,f]@.
weave :: [[a]] -> [a]
weave = concat . transpose

-- | Fold helper function that applies f to all but the last element, applies g to
-- last element and the accumulator.
foldle :: (a -> a -> a) -> (a -> a -> a) -> a -> [a] -> a
foldle _ _ z []     = z
foldle _ g z [x]    = g z x
foldle f g z [x,y]  = g (f z x) y
foldle f g z (x:xs) = foldle f g (f z x) xs

-- | Fold helper function that applies f to all but last element, applies g to last
-- element and accumulator without starting value, does not work for empty list.
foldle1 :: (a -> a -> a) -> (a -> a -> a) -> [a] -> a
foldle1 _ _ []       = error "foldle1 cannot be used with empty list"
foldle1 _ _ [x]      = x
foldle1 _ g [x,y]    = g x y
foldle1 f g (x:y:xs) = foldle f g (f x y) xs

-- | Convert "row" of elements into "column" of elements.
toColumn :: [a] -> [[a]]
toColumn = map (: [])
