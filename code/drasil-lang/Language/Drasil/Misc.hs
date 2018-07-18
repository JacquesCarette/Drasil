module Language.Drasil.Misc where

import Language.Drasil.Classes (HasUnitSymbol(usymb), NamedIdea(term), Idea)
import Language.Drasil.Chunk.NamedIdea (short)
import Language.Drasil.Chunk.Quantity (Quantity, getUnit)
import Language.Drasil.Chunk.Unitary (Unitary, unit)
import Language.Drasil.Spec ((+:+), Sentence((:+:), S, Sy), sParen)
import Language.Drasil.Development.UnitLang (USymb)

import qualified Language.Drasil.NounPhrase as NP

import Control.Lens ((^.))

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
mkTable _     []  = []
mkTable []     _  = error "Attempting to make table without data"
mkTable fl (c:cl) = map ($ c) fl : mkTable fl cl

-- where should this go?
-- | Get the units, if they exist, and wrap them as a Sentence
unitToSentence :: Quantity u => u -> Sentence
unitToSentence x = maybe (S "--") (\y -> Sy (y ^. usymb)) (getUnit x)

unitToSentenceUnitless :: Quantity u => u -> Sentence
unitToSentenceUnitless x = maybe (S "Unitless") (\y -> Sy (y ^. usymb)) (getUnit x)

-- | Helper for getting the unit's symbol from a chunk, 
-- as opposed to the symbols of the chunk itself.
unit_symb :: (Unitary c) => c -> USymb
unit_symb c = unit c ^. usymb

-- | Helper for common pattern of introducing the title-case version of a 
-- noun phrase (from a NamedIdea)
-- followed by its abbreviation in parentheses.
introduceAbb :: Idea n => n -> Sentence
introduceAbb n = NP.titleize (n ^. term) +:+ sParen (short n)

-- | Helper function for getting the sentence case of a noun phrase from a 
-- NamedIdea.
at_start, at_start' :: NamedIdea n => n -> Sentence
-- | Singular sentence case.
at_start  n = NP.at_start (n ^. term)
-- | Plural sentence case.
at_start' n = NP.at_start' (n ^. term)

-- | Helper function for getting the title case of a noun phrase from a 
-- NamedIdea.
titleize, titleize' :: NamedIdea n => n -> Sentence
-- | Singular title case.
titleize  n = NP.titleize (n ^. term)
-- | Plural title case.
titleize' n = NP.titleize' (n ^. term)

-- | Helper for getting the phrase from a NamedIdea.
phrase :: NamedIdea n => n -> Sentence
phrase n = NP.phrase (n ^. term)

-- | Helper for getting the plural of a phrase from a NamedIdea
plural :: NamedIdea n => n -> Sentence
plural n = NP.plural (n ^. term)

phrase's, plural's :: NamedIdea n => n -> Sentence
-- | Singular possesive function
phrase's a = phrase a :+: S "'s"
-- | Plural possesive function
plural's a = plural a :+: S "'"

-- Returns the string if it doesn't contain spaces and throws an error if it does
noSpaces :: String -> String
noSpaces s
  | not (' ' `elem` s) = s
  | otherwise          = error "String has at least one space in it."

{-
--------------------- WIP ---------------------
Function used to derive the unit of an equation. Takes a Relation, sorts the
respective values into lists of units found in the numerator and denominator,
eliminates units found in both list and combines the remainder to create the
units of the equation. WORK IN PROGRESS

inferUnit :: HasSymbolTable ctx => Relation -> ctx -> Maybe UnitDefn
inferUnit rel () = combine $ eliminate ([], []) $ convert symbtab $ findUnit rel ([], [])
  where combine (num, den) = 
          | 
          | otherwise = combine 

findUnit :: Relation -> ([SF], [SF]) -> ([SF], [SF])
findUnit (_ :+ a) ([], []) = analyze a True ([], [])
findUnit (_ :- a) ([], []) = analyze a True ([], [])
findUnit (a :* b) frac = findUnit a (analyze b True frac)
findUnit (a :/ b) frac = findUnit a (analyze b False frac)
findUnit (a :^ b) frac = error "Exponential not yet implemented"
findUnit (_ :+ _) frac = frac
findUnit (_ :- _) frac = frac
findUnit (_ := _) frac = frac

analyze :: Expr -> Bool -> ([SF], [SF]) -> ([SF], [SF])
analyze (Deriv _ (C a) (C b)) True (num, den) = (((SF a) ^. id):num, ((SF b) ^. id):den)
analyze (Deriv _ (C a) (C b)) False (num, den) = (((SF b) ^. id):num, ((SF a) ^. id):den)
analyze (FCall (C a) _) True (num, den) = (((SF a) ^. id):num, den)
analyze (FCall (C a) _) False (num, den) = (num, ((SF a) ^. id):den)
analyze (C a) True (num, den) = (((SF a) ^. id):num, den)
analyze (C b) False (num, den) = (num, ((SF b) ^. id):den)
analyze a True (num, den) = findUnit a (num, den)
analyze a False (num, den) = findUnit a (den, num)

convert :: HasSymbolTable ctx => ctx -> ([SF], [SF]) -> ([Maybe UnitDefn], [Maybe UnitDefn])
convert (num, den) = combine ((reorder (map (\x -> getUnitLup x symbtab) num)) ([], []), reorder (map (\x -> getUnitLup x symbtab) den) ([], []))
where reorder [] lst = lst
      reorder (frst:rst) lst = reorder rst (divide frst lst)
      divide (UPow a int) x@(val1, val2) = error "Exponential not yet implemented"
      divide (UDiv a b) lst = 
      divide (UProd a b) x@(val1, val2) =
      combine ((num1, den1), (num2, den2)) = (num1 ++ den2, den1 ++ num2)

eliminate :: [Maybe UnitDefn] -> ([Maybe UnitDefn], [Maybe UnitDefn]) -> ([Maybe UnitDefn], [Maybe UnitDefn])
eliminate lst ([], den) = (lst, den)
eliminate lst (frst:rst, den)
  | delete frst den == den = eliminate (frst:lst) (rst, den)
  | delete frst den /= den = eliminate lst (rst, delete frst den)
-}
