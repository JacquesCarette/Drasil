-- | Language for defining and manipulating units.
module Language.Drasil.UnitLang (
    -- * Types
    USymb(US), UDefn(..), UnitSymbol(BaseSI, DerivedSI, Defined)
    -- * Functions
  , fromUDefn, compUSymb, getUSymb, getDefn
  ) where

import Language.Drasil.Symbol (Symbol, compsy)
import Data.Foldable (foldl')

-- UName for the base cases, otherwise build up.
-- Probably a 7-vector would be better (less error-prone!)
-- | Language of units (how to build them up into a unit symbol).
-- Of the form ('Symbol' ^ 'Integer'). The 'Integer' may be negative, but should not be zero.
newtype USymb = US [(Symbol, Integer)] -- can be negative, should not be 0
  deriving (Eq)

-- | Language of unit equations, to define a unit relative
-- to another.
data UDefn = USynonym USymb      -- ^ to define straight synonyms.
           | UScale Double USymb -- ^ scale, i.e. *.
           | UShift Double USymb -- ^ shift, i.e. +.

-- | Generates a default unit symbol.
fromUDefn :: UDefn -> USymb
fromUDefn (USynonym x) = x
fromUDefn (UScale _ s) = s
fromUDefn (UShift _ s) = s

-- | Hand-rolled version of compare. Should assume 'USymb' is normalized, so
-- that some redundant EQ cases can be removed.
compUSymb :: USymb -> USymb -> Ordering
compUSymb (US l)  (US m)  = foldl' mappend EQ $ zipWith comp l m
  where
    comp (s1, i1) (s2, i2) = compsy s1 s2 `mappend` compare i1 i2

-- | When we define units, they come in three flavours:
-- SI (base) units, derived SI units (aka synonyms), and defined units.
-- The type below captures that knowledge.
data UnitSymbol =
     BaseSI USymb
   | DerivedSI USymb USymb UDefn
   | Defined USymb UDefn

-- | Generates a default unit symbol.
getUSymb :: UnitSymbol -> USymb
getUSymb (BaseSI u) = u
getUSymb (DerivedSI u _ _) = u
getUSymb (Defined u _) = u

-- | Gets the unit definition of a unit symbol.
getDefn :: UnitSymbol -> Maybe UDefn
getDefn (BaseSI _) = Nothing
getDefn (DerivedSI _ _ d) = Just d
getDefn (Defined _ d) = Just d
