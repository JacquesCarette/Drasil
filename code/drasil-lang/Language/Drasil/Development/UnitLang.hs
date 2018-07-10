module Language.Drasil.Development.UnitLang (
    USymb(US), UDefn(..)
  , from_udefn, comp_usymb
  ) where

import Language.Drasil.Symbol (Symbol, compsy)

-- | Language of units (how to build them up)
-- UName for the base cases, otherwise build up.
-- Probably a 7-vector would be better (less error-prone!)
-- | Language of unit equations, to define a unit relative
-- to another
newtype USymb = US [(Symbol, Integer)] -- can be negative, should not be 0
  deriving (Eq)

data UDefn = FUSynonym USymb
		   | FUShift Double USymb
		   | FUScale Double USymb
		   | USynonym USymb      -- ^ to define straight synonyms
       | UScale Double USymb -- ^ scale, i.e. *
       | UShift Double USymb -- ^ shift, i.e. +

-- | Can generate a default symbol
from_udefn :: UDefn -> USymb
from_udefn (USynonym x) = x
from_udefn (UScale _ s) = s
from_udefn (UShift _ s) = s
from_udefn (FUSynonym x) = x
from_udefn (FUShift _ s) = s
from_udefn (FUScale _ s) = s  

-- | Hand-rolled version of compare. Should assume |USymb| is normalized, so
-- that some redundant EQ cases can be removed.
comp_usymb :: USymb -> USymb -> Ordering
comp_usymb (US l)  (US m)  = foldl mappend EQ $ zipWith comp l m
  where
    comp (s1, i1) (s2, i2) = compsy s1 s2 `mappend` compare i1 i2
