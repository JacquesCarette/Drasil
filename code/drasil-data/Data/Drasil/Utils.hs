{-# Language TypeFamilies #-}
module Data.Drasil.Utils
  ( foldle
  , foldle1
  , mkEnumAbbrevList
  , zipFTable'
  , zipSentList
  , makeTMatrix
  , itemRefToSent
  , noRefs
  , noRefsLT
  , makeListRef
  , bulletFlat
  , bulletNested
  , enumSimple
  , enumBullet
  , enumSimpleU
  , enumBulletU
  , mkInputDatTb
  , getRVal
  , addPercent
  , weave
  , fmtU
  , unwrap
  , fterms
  , prodUCTbl
  , eqUnR, eqUnR'
  ) where

import Language.Drasil
import Language.Drasil.Development (UnitDefn, MayHaveUnit(getUnit))

import Control.Lens ((^.))
import Data.List (transpose, elem)

import Data.Drasil.Concepts.Documentation (fterms, input_, output_, symbol_, 
  useCaseTable)
import Data.Drasil.Concepts.Math (unit_)

eqUnR :: Expr -> Reference -> LabelledContent
eqUnR e lbl = llcc lbl $ EqnBlock e

eqUnR' :: Expr -> Contents
eqUnR' e = UlC $ ulcc $ EqnBlock e

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
mkEnumAbbrevList s t l = zip (enumWithAbbrev s t) $ map Flat l

-- | takes a amount and adds a unit to it
-- n - sentenc representing an amount
-- u - unit we want to attach to amount
fmtU :: (Quantity a, MayHaveUnit a) => Sentence -> a -> Sentence
fmtU n u  = n +:+ (unwrap $ getUnit u)

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

zipFTable' :: Eq a => [a] -> [a] -> [Sentence]
zipFTable' content l = concatMap (\x -> if x `elem` content then [S "X"] else [EmptyS]) l

-- | makes a traceability matrix from list of column rows and list of rows
makeTMatrix :: Eq a => [Sentence] -> [[a]] -> [a] -> [[Sentence]]
makeTMatrix colName col row = zipSentList [] colName [zipFTable' x row | x <- col] 


-- | takes a list of wrapped variables and creates an Input Data Table for uses in Functional Requirments
mkInputDatTb :: (Quantity a, MayHaveUnit a) => [a] -> LabelledContent
mkInputDatTb inputVar = llcc (makeTabRef "inDataTable") $ 
  Table [titleize symbol_, titleize unit_, 
  S "Name"]
  (mkTable [ch , fmtU EmptyS, phrase] inputVar) 
  (S "Required" +:+ titleize' input_) True

-- | makes sentences from an item and its reference 
-- a - String title of reference
-- b - Sentence containing the full reference
itemRefToSent :: String -> Sentence -> Sentence
itemRefToSent a b = S a +:+ sParen b

-- | makeListRef takes a list and a reference and generates references to 
--   match the length of the list
-- l - list whos length is to be matched
-- r - reference to be repeated
makeListRef :: [a] -> Section -> [Sentence]
makeListRef l r = take (length l) $ repeat $ Ref $ makeRef2 r

-- | bulletFlat applies Bullet and Flat to a list.
bulletFlat :: [Sentence] -> ListType
bulletFlat = Bullet . noRefs . map Flat

-- | bulletNested applies Bullets and headers to a Nested ListType.
-- t - Headers of the Nested lists.
-- l - Lists of ListType.
bulletNested :: [Sentence] -> [ListType] -> ListType
bulletNested t l = Bullet . map (\(h,c) -> (Nested h c, Nothing)) $ zip t l

-- | enumBullet apply Enumeration, Bullet and Flat to a list
enumBullet :: Reference -> [Sentence] -> LabelledContent --FIXME: should Enumeration be labelled?
enumBullet lb s = llcc lb $ Enumeration $ bulletFlat s

enumBulletU ::[Sentence] -> Contents --FIXME: should Enumeration be labelled?
enumBulletU s =  UlC $ ulcc $ Enumeration $ bulletFlat s

-- | enumSimple enumerates a list and applies simple and enumeration to it
-- s - start index for the enumeration
-- t - title of the list
-- l - list to be enumerated
enumSimple :: Reference -> Integer -> Sentence -> [Sentence] -> LabelledContent --FIXME: should Enumeration be labelled?
enumSimple lb s t l = llcc lb $ Enumeration $ Simple $ noRefsLT $ mkEnumAbbrevList s t l

enumSimpleU :: Integer -> Sentence -> [Sentence] -> Contents --FIXME: should Enumeration be labelled?
enumSimpleU s t l = UlC $ ulcc $ Enumeration $ Simple $ noRefsLT $ mkEnumAbbrevList s t l

-- | interweaves two lists together [[a,b,c],[d,e,f]] -> [a,d,b,e,c,f]
weave :: [[a]] -> [a]
weave = concat . transpose

-- | get a unit symbol if there is one
unwrap :: (Maybe UnitDefn) -> Sentence
unwrap (Just a) = Sy $ usymb a
unwrap Nothing  = EmptyS

-- | noRefs converts lists of simple ItemTypes into a lists which may be used
-- in Contents but not directly referable.
noRefs :: [ItemType] -> [(ItemType, Maybe RefAdd)]
noRefs a = zip a $ repeat Nothing

-- | noRefsLT converts lists of tuples containing a title and ItemType into
-- a ListTuple which can be used with Contents but not directly referable.
noRefsLT :: [(Sentence, ItemType)] -> [ListTuple]
noRefsLT a = uncurry zip3 (unzip a) $ repeat Nothing

prodUCTbl :: [[Sentence]] -> LabelledContent
prodUCTbl cases = llcc (makeTabRef "useCaseTable") $ --FIXME: do we want labels across examples to be unique?
  Table [S "Actor", titleize input_ +:+ S "and" +:+ titleize output_]
  cases (titleize useCaseTable) True
