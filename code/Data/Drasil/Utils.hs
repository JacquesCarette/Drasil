module Data.Drasil.Utils
  ( foldle
  , foldle1
  , foldlSent
  , foldlList
  , foldlsC
  , mkEnumAbbrevList
  , listConstS
  , listConstUC
  , zipFTable
  , zipSentList
  , makeTMatrix
  , itemRefToSent
  , refFromType
  , makeListRef
  , enumSimple
  , enumBullet
  , mkRefsList
  ) where

import Control.Lens ((^.))
import Language.Drasil (Sentence(Sy, P, EmptyS, S, (:+:)), (+:+), (+:+.), 
  ItemType(Flat), sC, sParen, sSqBr, Contents(Definition, Enumeration), 
  makeRef, DType, Section, ListType(Simple, Bullet), UnitalChunk, 
  unit_symb, symbol, SymbolForm, Unitary, SymbolMap)
  
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

-- | partial function application of foldle for sentences specifically
foldlSent :: [Sentence] -> Sentence
foldlSent = foldle (+:+) (+:+.) EmptyS

-- | creates a list of elements seperated by commas, ending in a "_, and _"
foldlList :: [Sentence] -> Sentence
foldlList = foldle1 sC (\a b -> a `sC` S "and" +:+ b)

-- | creates a list of elements seperated by commas, including the last element
foldlsC :: [Sentence] -> Sentence
foldlsC []       = EmptyS
foldlsC [x]      = x
foldlsC [x,y]    = x `sC` y
foldlsC (x:y:xs) = foldle sC sC (x `sC` y) xs

-- | concantenates number to abbreviation
-- should not be exported
enumWithAbbrev :: Integer -> Sentence -> [Sentence]
enumWithAbbrev start abbrev = [abbrev :+: (S $ show x) | x <- [start..]]

-- | zip helper function enumerates abbreviation and zips it with list of itemtype
-- s - the number from which the enumeration should start from
-- t - the title of the list
-- l - the list to be enumerated
mkEnumAbbrevList :: Integer -> Sentence -> [Sentence] -> [(Sentence, ItemType)]
mkEnumAbbrevList s t l = zip (enumWithAbbrev s t) (map (Flat) l)

mkRefsList :: Integer -> [Sentence] -> Contents
mkRefsList s l = Enumeration $ Simple $ zip (enumWithSquBrk s) (map (Flat) l)

enumWithSquBrk :: Integer -> [Sentence]
enumWithSquBrk start = [sSqBr $ S $ show x | x <- [start..]]

-- | formats constraints on variables for tables
fmtCS :: Sentence -> Sentence -> Sentence -> Sentence
fmtCS _ EmptyS EmptyS = S "None"  
fmtCS symb a EmptyS   = symb +:+ a
fmtCS symb a b        = symb +:+ a +:+ S "and" +:+ symb +:+ b

-- | formats numbers with units for tables
fmtUS :: Sentence -> Sentence -> Sentence
fmtUS num EmptyS = num
fmtUS num units  = num +:+ units

-- | takes a amount and adds a unit to it
fmtU :: (Unitary a) => Sentence -> a -> Sentence
fmtU n u  = n +:+ getU u

-- | takes a chunk and constraints and makes a sentence of the constraints
-- on that chunk
fmtC ::(SymbolForm a) => a -> [Sentence] -> Sentence
fmtC _ []      = S "None"  
fmtC symb [x]  = (getS symb) +:+ x
fmtC symb (x:xs) = (getS symb) +:+ x +:+ S "and" +:+ (fmtC symb xs)


getS :: (SymbolForm a) => a -> Sentence
-- | gets symbol from chunk
getS s  = P $ s ^. symbol
-- | gets unit from chunk
getU :: (Unitary a) => a -> Sentence
getU s = Sy $ unit_symb s

-- | makes a list of sentence from sentences
listConstS :: (Sentence, Sentence, Sentence, Sentence, Sentence) -> [Sentence]
listConstS (symb, a, b, n, u) = [symb, fmtCS symb a b, fmtUS n u]

-- | makes a list of sentence from unital chunk and a constraint list with units
listConstUC :: (Unitary a, SymbolForm a) => (a, [Sentence], Sentence) -> [Sentence]
listConstUC (s, a, b) = [getS s, fmtC s a, fmtU b s]

-- | appends a sentence to the front of a list of list of sentences
zipSentList :: [[Sentence]] -> [Sentence] -> [[Sentence]] -> [[Sentence]] 
zipSentList acc _ []           = acc
zipSentList acc [] r           = acc ++ (map (EmptyS:) r)
zipSentList acc (x:xs) (y:ys)  = zipSentList (acc ++ [x:y]) xs ys

-- | traceability matrices row from a list of rows and a list of columns
-- acc - accumulator
-- k   - list of type that is comparable
-- l   - list of type that is comparable
zipFTable :: Eq a => [Sentence] -> [a] -> [a] -> [Sentence]
zipFTable acc _ []              = acc
zipFTable acc [] l              = acc ++ (take (length l) (repeat EmptyS))
zipFTable acc k@(x:xs) (y:ys)   | x == y    = zipFTable (acc++[S "X"]) xs ys
                                | otherwise = zipFTable (acc++[EmptyS]) k ys

-- | makes a traceability matrix from list of column rows and list of rows
makeTMatrix :: Eq a => [Sentence] -> [[a]] -> [a] -> [[Sentence]]
makeTMatrix colName col row = zipSentList [] colName [zipFTable [] x row | x <- col] 

-- | makes sentences from an item and its reference 
-- a - String title of reference
-- b - Sentence containing the full reference
itemRefToSent :: String -> Sentence -> Sentence
itemRefToSent a b = S a +:+ sParen b

-- | refFromType takes a function and returns a reference sentence
refFromType :: (a -> DType) -> SymbolMap -> a -> Sentence
refFromType f m = (makeRef . Definition m . f)

-- | makeListRef takes a list and a reference and generates references to 
--   match the length of the list
-- l - list whos length is to be matched
-- r - reference to be repeated
makeListRef :: [a] -> Section -> [Sentence]
makeListRef l r = take (length l) $ repeat $ makeRef r


-- | enumBullet apply Enumeration, Bullet and Flat to a list
enumBullet ::[Sentence] -> Contents
enumBullet f = Enumeration $ Bullet $ map (Flat) f

-- | enumSimple enumerates a list and applies simple and enumeration to it
-- s - start index for the enumeration
-- t - title of the list
-- l - list to be enumerated
enumSimple :: Integer -> Sentence -> [Sentence] -> Contents
enumSimple s t l = Enumeration $ Simple $ mkEnumAbbrevList s t l