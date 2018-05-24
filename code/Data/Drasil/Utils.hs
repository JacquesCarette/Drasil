{-# Language TypeFamilies #-}
module Data.Drasil.Utils
  ( foldle
  , foldle1
  , mkEnumAbbrevList
  , zipFTable
  , zipSentList
  , makeTMatrix
  , itemRefToSent
  , refFromType
  , makeListRef
  , enumSimple
  , enumBullet
  , mkRefsList
  , mkInputDatTb
  , getES
  , getRVal
  , addPercent
  , weave
  , fmtU
  , unwrap
  , fterms
  , mkDataDef, mkDataDef'
  , prodUCTbl
  , eqUnR
  ) where

import Data.List
import Control.Lens ((^.))
import Language.Drasil {-(Sentence(Sy, P, EmptyS, S, (:+:), E), (+:+),
  ItemType(Flat), sParen, sSqBr, Contents(Definition, Enumeration), 
  makeRef, DType, Section, ListType(Simple, Bullet), getUnit, Quantity,
  symbol, SymbolForm, symbolMap, UnitDefn, usymb, Chunk, Expr(..),
  phrase, titleize, titleize', mkTable, Contents(Table), fromEqn, fromEqn', 
  UnitalChunk, QDefinition, term, uid, unit, ucw)-}
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (unit_)

eqUnR :: Expr -> Contents -- FIXME: Unreferable equations
eqUnR e = EqnBlock e ""
  
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

-- | concantenates number to abbreviation
-- should not be exported
enumWithAbbrev :: Integer -> Sentence -> [Sentence]
enumWithAbbrev start abbrev = [abbrev :+: (S $ show x) | x <- [start..]]

-- | zip helper function enumerates abbreviation and zips it with list of itemtype
-- s - the number from which the enumeration should start from
-- t - the title of the list
-- l - the list to be enumerated
mkEnumAbbrevList :: Integer -> Sentence -> [Sentence] -> [(Sentence, ItemType)]
mkEnumAbbrevList s t l = zip (enumWithAbbrev s t) (map Flat l)

-- | creates a list of references from l starting from s
-- s - start indices
-- l - list of references
mkRefsList :: Integer -> [Sentence] -> Contents
mkRefsList s l = Enumeration $ Simple $ zip (enumWithSquBrk s) (map Flat l)

-- | creates a list of sentences of the form "[#]"
-- start - start indices
enumWithSquBrk :: Integer -> [Sentence]
enumWithSquBrk start = [sSqBr $ S $ show x | x <- [start..]]

-- | takes a amount and adds a unit to it
-- n - sentenc representing an amount
-- u - unit we want to attach to amount
fmtU :: (Quantity a) => Sentence -> a -> Sentence
fmtU n u  = n +:+ (unwrap $ getUnit u)

-- | gets 'presentation' symbol from chunk
getES :: (HasSymbol a) => a -> Sentence
getES = P . eqSymb

-- | gets a reasonable or typical value from a Constrained chunk
getRVal :: (HasUID c, HasReasVal c) => c -> Expr
getRVal c = uns (c ^. reasVal)
  where uns (Just e) = e
        uns Nothing  = error $ "getRVal found no Expr for " ++ (c ^. uid)

-- | outputs sentence with % attached to it
addPercent :: Float ->  Sentence
addPercent num = (S (show num) :+: (Sp Percent))

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

-- | takes a list of wrapped variables and creates an Input Data Table for uses in Functional Requirments
mkInputDatTb :: (Quantity a) => [a] -> Contents
mkInputDatTb inputVar = Table [titleize symbol_, titleize unit_, 
  S "Name"]
  (mkTable [getES , fmtU EmptyS, phrase] inputVar) 
  (S "Required" +:+ titleize' input_) True "inDataTable"

-- | makes sentences from an item and its reference 
-- a - String title of reference
-- b - Sentence containing the full reference
itemRefToSent :: String -> Sentence -> Sentence
itemRefToSent a b = S a +:+ sParen b

-- | refFromType takes a function and returns a reference sentence
refFromType :: (a -> DType) -> a -> Sentence
refFromType f = (makeRef . Definition . f)

-- | makeListRef takes a list and a reference and generates references to 
--   match the length of the list
-- l - list whos length is to be matched
-- r - reference to be repeated
makeListRef :: [a] -> Section -> [Sentence]
makeListRef l r = take (length l) $ repeat $ makeRef r


-- | enumBullet apply Enumeration, Bullet and Flat to a list
enumBullet ::[Sentence] -> Contents
enumBullet = Enumeration . Bullet . map Flat

-- | enumSimple enumerates a list and applies simple and enumeration to it
-- s - start index for the enumeration
-- t - title of the list
-- l - list to be enumerated
enumSimple :: Integer -> Sentence -> [Sentence] -> Contents
enumSimple s t l = Enumeration $ Simple $ mkEnumAbbrevList s t l

-- | interweaves two lists together [[a,b,c],[d,e,f]] -> [a,d,b,e,c,f]
weave :: [[a]] -> [a]
weave = concat . transpose

-- | get a unit symbol if there is one
unwrap :: (Maybe UnitDefn) -> Sentence
unwrap (Just a) = Sy (a ^. usymb)
unwrap Nothing  = EmptyS

-- Used to help make Qdefinitions when uid, term, and symbol come from the same source
mkDataDef :: (Quantity c) => c -> Expr -> QDefinition
mkDataDef cncpt equation = datadef $ getUnit cncpt --should references be passed in at this point?
  where datadef (Just a) = fromEqn  (cncpt ^. uid) (cncpt ^. term) EmptyS
                           (eqSymb cncpt) a equation [] [] 
        datadef Nothing  = fromEqn' (cncpt ^. uid) (cncpt ^. term) EmptyS
                           (eqSymb cncpt) equation [] []

-- Same as 'mkDataDef', but with an additional Sentence that can be taken as "extra information"; issue #350
mkDataDef' :: (Quantity c) => c -> Expr -> Sentence -> Attributes -> References -> QDefinition
mkDataDef' cncpt equation extraInfo atts refs = datadef $ getUnit cncpt
  where datadef (Just a) = fromEqn  (cncpt ^. uid) (cncpt ^. term) (extraInfo)
                           (eqSymb cncpt) a equation atts refs
        datadef Nothing  = fromEqn' (cncpt ^. uid) (cncpt ^. term) (extraInfo)
                           (eqSymb cncpt) equation atts refs

prodUCTbl :: [[Sentence]] -> Contents
prodUCTbl cases = Table [S "Actor", titleize input_ +:+ S "and" +:+ titleize output_]
  cases
  (titleize useCaseTable) True "useCaseTable"

