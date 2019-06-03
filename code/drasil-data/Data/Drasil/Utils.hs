{-# Language TypeFamilies #-}
module Data.Drasil.Utils (addPercent, bulletFlat, bulletNested, enumBullet,
  enumBulletU, enumSimple, enumSimpleU, eqUnR, eqUnR', fmtPhys, fmtSfwr, fmtU,
  fterms, getRVal, itemRefToSent, makeListRef, makeTMatrix, mkEnumAbbrevList,
  mkInputDatTb, mkTableFromColumns, noRefs, noRefsLT, typUncr, unwrap, weave,
  zipFTable', zipSentList) where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (fterms, input_, symbol_)
import Data.Drasil.Concepts.Math (unit_)

import Control.Lens ((^.))
import Data.Decimal (DecimalRaw, realFracToDecimal)
import Data.List (elem, transpose)

eqUnR :: Expr -> Reference -> LabelledContent
eqUnR e lbl = llcc lbl $ EqnBlock e

eqUnR' :: Expr -> Contents
eqUnR' e = UlC $ ulcc $ EqnBlock e

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
fmtU :: (MayHaveUnit a) => Sentence -> a -> Sentence
fmtU n u  = n +:+ unwrap (getUnit u)

-- | formats physical constraints
fmtPhys :: (Constrained c, Quantity c) => c -> Sentence
fmtPhys c = foldConstraints c $ filter isPhysC (c ^. constraints)

-- | formats software constraints
fmtSfwr :: (Constrained c, Quantity c) => c -> Sentence
fmtSfwr c = foldConstraints c $ filter isSfwrC (c ^. constraints)

-- | gets a reasonable or typical value from a Constrained chunk
getRVal :: (HasUID c, HasReasVal c) => c -> Expr
getRVal c = uns (c ^. reasVal)
  where uns (Just e) = e
        uns Nothing  = error $ "getRVal found no Expr for " ++ (c ^. uid)

-- | extracts the typical uncertainty to be displayed from something that has an uncertainty
typUncr :: HasUncertainty c => c -> Sentence
typUncr x = found (uncVal x) (uncPrec x)
  where
    found u Nothing  = addPercent $ u * 100
    found u (Just p) = addPercent (realFracToDecimal (fromIntegral p) (u * 100) :: DecimalRaw Integer)

-- | outputs sentence with % attached to it
addPercent :: Show a => a -> Sentence
addPercent num = (S (show num) :+: Percent)

-- | appends a sentence to the front of a list of list of sentences
zipSentList :: [[Sentence]] -> [Sentence] -> [[Sentence]] -> [[Sentence]] 
zipSentList acc _ []           = acc
zipSentList acc [] r           = acc ++ (map (EmptyS:) r)
zipSentList acc (x:xs) (y:ys)  = zipSentList (acc ++ [x:y]) xs ys

-- | traceability matrices row from a list of rows and a list of columns

zipFTable' :: Eq a => [a] -> [a] -> [Sentence]
zipFTable' content = concatMap (\x -> if x `elem` content then [S "X"] else [EmptyS])

-- | makes a traceability matrix from list of column rows and list of rows
makeTMatrix :: Eq a => [Sentence] -> [[a]] -> [a] -> [[Sentence]]
makeTMatrix colName col row = zipSentList [] colName [zipFTable' x row | x <- col] 

-- | Helper for making a table from a columns
mkTableFromColumns :: [(Sentence, [Sentence])] -> ([Sentence], [[Sentence]])
mkTableFromColumns l = 
  let l' = filter (any (not . isEmpty) . snd) l in 
  (map fst l', transpose $ map ((map replaceEmptyS) . snd) l')
  where
    isEmpty       EmptyS = True
    isEmpty       _      = False
    replaceEmptyS EmptyS = S "--"
    replaceEmptyS s      = s

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
makeListRef :: [a] -> Section -> [Sentence]
makeListRef l = replicate (length l) . Ref . makeRef2

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
noRefs :: [ItemType] -> [(ItemType, Maybe String)]
noRefs a = zip a $ repeat Nothing

-- | noRefsLT converts lists of tuples containing a title and ItemType into
-- a ListTuple which can be used with Contents but not directly referable.
noRefsLT :: [(Sentence, ItemType)] -> [ListTuple]
noRefsLT a = uncurry zip3 (unzip a) $ repeat Nothing
