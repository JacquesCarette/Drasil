{-# Language TypeFamilies #-}
module Utils.Drasil.Fold (EnumType(..), WrapType(..), SepType(..),
  FoldType(..), foldConstraints, foldlEnumList, foldlList, foldlSP,
  foldlSP_, foldlSPCol, foldlSent, foldlSent_, foldlSentCol, foldlsC) where

import Language.Drasil
import Utils.Drasil.Sentence (sAnd, sOr)

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

{--** Sentence Folding **--}
-- | partial function application of foldle for sentences specifically
foldlSent :: [Sentence] -> Sentence
foldlSent = foldle (+:+) (+:+.) EmptyS

-- | foldlSent but does not end with period
foldlSent_ :: [Sentence] -> Sentence
foldlSent_ = foldl (+:+) EmptyS

-- | foldlSent but ends with colon
foldlSentCol :: [Sentence] -> Sentence
foldlSentCol = foldle (+:+) (+:) EmptyS

-- | fold sentences then turns into content
foldlSP :: [Sentence] -> Contents
foldlSP = mkParagraph . foldlSent

foldlSP_ :: [Sentence] -> Contents
foldlSP_ = mkParagraph . foldlSent_

foldlSPCol :: [Sentence] -> Contents
foldlSPCol = mkParagraph . foldlSentCol

-- | creates a list of elements separated by commas, including the last element
foldlsC :: [Sentence] -> Sentence 
foldlsC [] = EmptyS
foldlsC xs = foldl1 sC xs

data EnumType = Numb   | Upper   | Lower
data WrapType = Parens | Period
data SepType  = Comma  | SemiCol
data FoldType = List   | Options

-- | creates an list of elements with "enumerators" in "wrappers" using foldlList
foldlEnumList :: EnumType -> WrapType -> SepType -> FoldType -> [Sentence] -> Sentence
foldlEnumList e w s l lst = foldlList s l $ zipWith (+:+) (numList e w $ length lst) lst
  where
    numList enum wt len = map (\x -> wrap wt $ S x) (take len (chList enum))
    chList Numb  = map show ([1..] :: [Integer])
    chList Upper = map show ['A'..'Z']
    chList Lower = map show ['a'..'z']
    wrap Parens x = sParen x
    wrap Period x = x :+: S "."

-- | creates a list of elements separated by a "separator", ending with "and" or "or"
foldlList :: SepType -> FoldType -> [Sentence] -> Sentence
foldlList _ _ []     = EmptyS
foldlList _ f [a, b] = end f a b
foldlList s f lst    = foldle1 (sep s) (\a b -> end f ((sep s) a EmptyS) b) lst

--Helper functions to foldlList - not exported
end :: FoldType -> (Sentence -> Sentence -> Sentence)
end List    = sAnd
end Options = sOr

sep :: SepType -> (Sentence -> Sentence -> Sentence)
sep Comma   = sC
sep SemiCol = (\a b -> a :+: S ";" +:+ b)
