module Data.Drasil.Utils
  ( foldle
  , foldle1
  , foldlSent
  , foldlList
  , foldlsC
  , mkEnumAbbrevList
  , listConstS
  , listConstExpr
  , zipFTable
  , zipSentList
  , makeTMatrix
  , itemRefToSent
  , refFromType
  , makeListRef
  , enumSimple
  , enumBullet
  , mkRefsList
  , ofThe, ofThe'
  , getS
  , weave
  , fmtU
  , unwrap
  , isThe
  , fmtBF
  , symbolMapFun
  ) where

import Data.List
import Control.Lens ((^.))
import Language.Drasil (Sentence(Sy, P, EmptyS, S, (:+:), E), (+:+), (+:+.), 
  ItemType(Flat), sC, sParen, sSqBr, Contents(Definition, Enumeration), 
  makeRef, DType, Section, ListType(Simple, Bullet), getUnit, Quantity,
  symbol, SymbolForm, SymbolMap, symbolMap, UnitDefn, usymb, Expr(..))
  
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
foldlList []    = EmptyS
foldlList [a,b] = a +:+ S "and" +:+ b
foldlList lst   = foldle1 sC (\a b -> a `sC` S "and" +:+ b) lst

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
fmtU :: (Quantity a, SymbolForm a) => Sentence -> a -> Sentence
fmtU n u  = n +:+ (unwrap $ getUnit u)

-- | takes a chunk and a list of binary operator contraints to make an expression (Sentence)
-- ex. fmtBF x [((:>),0), ((:<),1)] -> x>0 and x<1
fmtBF ::(SymbolForm a) => a -> [(Expr -> Expr -> Expr, Expr)] -> Sentence
fmtBF _ []      = S "None"  
fmtBF symb [(f,num)]  = E ((C symb) `f` num)
fmtBF symb ((f,num):xs) = (E ((C symb) `f` num)) +:+ S "and" +:+ (fmtBF symb xs)

-- | makes a constraint table entry from symbol expr and sentence
listConstExpr :: (SymbolForm a, Quantity a) => (a, [(Expr -> Expr -> Expr, Expr)], Sentence) -> [Sentence]
listConstExpr (s, a, b) = [getS s, fmtBF s a, fmtU b s]

-- | gets symbol from chunk
getS :: (SymbolForm a) => a -> Sentence
getS s  = P $ s ^. symbol

-- | makes a list of sentence from sentences
listConstS :: (Sentence, Sentence, Sentence, Sentence, Sentence) -> [Sentence]
listConstS (symb, a, b, n, u) = [symb, fmtCS symb a b, fmtUS n u]

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

-- | interweaves two lists together [[a,b,c],[d,e,f]] -> [a,d,b,e,c,f]
weave :: [[a]] -> [a]
weave = (concat . transpose)

--combinator functions that are used by introF in OrganizationOfSRS
sAnd :: Sentence -> Sentence -> Sentence
sAnd p1 p2 = p1 +:+ S "and" +:+ p2

andIts :: Sentence -> Sentence -> Sentence
andIts p1 p2 = p1 +:+ S "and its" +:+ p2

andThe :: Sentence -> Sentence -> Sentence
andThe p1 p2 = p1 +:+ S "and the" +:+ p2

sAre :: Sentence -> Sentence -> Sentence
sAre p1 p2 = p1 +:+ S "are" +:+ p2

sIn :: Sentence -> Sentence -> Sentence
sIn p1 p2 = p1 +:+ S "in" +:+ p2

sIs :: Sentence -> Sentence -> Sentence
sIs p1 p2 = p1 +:+ S "is" +:+ p2

isThe :: Sentence -> Sentence -> Sentence
isThe p1 p2 = p1 +:+ S "is the" +:+ p2

sOf :: Sentence -> Sentence -> Sentence
sOf p1 p2 = p1 +:+ S "of" +:+ p2

sOr :: Sentence -> Sentence -> Sentence
sOr p1 p2 = p1 +:+ S "or" +:+ p2

ofThe, ofThe' :: Sentence -> Sentence -> Sentence
ofThe  p1 p2 = S "the" +:+ p1 +:+ S "of the" +:+ p2
ofThe' p1 p2 = S "The" +:+ p1 +:+ S "of the" +:+ p2

toThe :: Sentence -> Sentence -> Sentence
toThe p1 p2 = p1 +:+ S "to the" +:+ p2

unwrap :: (Maybe UnitDefn) -> Sentence
unwrap (Just a) = Sy (a ^. usymb)
unwrap Nothing  = EmptyS

-- Using symbolMap from Extract
--FIXME: Not sure what type d should be
symbolMapFun :: (SymbolForm c, Quantity c) => [c] -> (d -> DType) -> (d -> Contents)
symbolMapFun progSymbMap fun = (Definition (symbolMap progSymbMap) . fun)



