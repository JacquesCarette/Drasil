{-# Language TypeFamilies #-}
module Utils.Drasil.Fold (foldle, foldle1, foldConstraints) where

import Language.Drasil

-- | fold helper functions applies f to all but the last element, applies g to
-- last element and the accumulator
foldle :: (a -> a -> a) -> (a -> a -> a) -> a -> [a] -> a
foldle _ _ z []     = z
foldle _ g z [x]    = g z x
foldle f g z [x,y]  = g (f z x) y
foldle f g z (x:xs) = foldle f g (f z x) xs

-- | fold helper functions applied f to all but last element, applies g to last
-- element and accumulator without starting value, does not work for empty list
foldle1 :: (a -> a -> a) -> (a -> a -> a) -> [a] -> a
foldle1 _ _ []       = error "foldle1 cannot be used with empty list"
foldle1 _ _ [x]      = x
foldle1 _ g [x,y]    = g x y
foldle1 f g (x:y:xs) = foldle f g (f x y) xs

-- | helper for formatting constraints
foldConstraints :: (Quantity c) => c -> [Constraint] -> Sentence
foldConstraints _ [] = EmptyS
foldConstraints c e  = E $ foldl1 ($&&) $ map constraintToExpr e
  where
    constraintToExpr (Range _ ri)         = real_interval c ri
    constraintToExpr (EnumeratedReal _ l) = isin (sy c) (DiscreteD l)
    constraintToExpr (EnumeratedStr _ l)  = isin (sy c) (DiscreteS l)
