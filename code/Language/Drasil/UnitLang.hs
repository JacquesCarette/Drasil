module Language.Drasil.UnitLang (
    USymb(UName,UProd,UPow,UDiv), UDefn(..)
  , from_udefn, comp_usymb
  ) where

import Language.Drasil.Symbol (Symbol, compsy)

-- | Language of units (how to build them up)
-- UName for the base cases, otherwise build up.
-- Probably a 7-vector would be better (less error-prone!)
-- | Language of unit equations, to define a unit relative
-- to another
data USymb = UName Symbol
           | UProd [USymb] -- ^ Product
           | UPow USymb Integer -- ^ can be negative, should not be 0
           | UDiv USymb USymb   -- ^ Get proper division (not neg pow)
                                -- necessary for things like J/(kg*C)
  deriving (Eq)


data UDefn = USynonym USymb      -- ^ to define straight synonyms
           | UScale Double USymb -- ^ scale, i.e. *
           | UShift Double USymb -- ^ shift, i.e. +

-- | Can generate a default symbol
from_udefn :: UDefn -> USymb
from_udefn (USynonym x) = x
from_udefn (UScale _ s) = s
from_udefn (UShift _ s) = s

-- | Hand-rolled version of compare. Should assume |USymb| is normalized, so
-- that some redundant EQ cases can be removed.
comp_usymb :: USymb -> USymb -> Ordering
comp_usymb (UName a)  (UName b)  = compsy a b
comp_usymb (UName _)  _          = LT
comp_usymb (UProd l)  (UProd m)  = foldl mappend EQ $ zipWith comp_usymb l m
comp_usymb (UProd l)  (UName b)  = if l == [UName b] then EQ else GT
comp_usymb (UProd _)  _          = LT
comp_usymb (UPow l a) (UPow m b) = case comp_usymb l m of { EQ -> compare a b; x -> x}
comp_usymb (UPow l a) (UName b)  = if l == UName b && a == 1 then EQ else GT
comp_usymb (UPow _ _) (UProd _)  = GT
comp_usymb (UPow _ _) _          = LT
comp_usymb (UDiv a b) (UDiv c d) = comp_usymb a c `mappend` comp_usymb b d
comp_usymb (UDiv _ _) _          = GT
