module Language.Drasil.UnitLang (
    USymb(..), UDefn(..)
  , from_udefn
  ) where

import Language.Drasil.Symbol (Symbol)

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
  deriving (Eq, Ord)


data UDefn = USynonym USymb      -- ^ to define straight synonyms
           | UScale Double USymb -- ^ scale, i.e. *
           | UShift Double USymb -- ^ shift, i.e. +

-- | Can generate a default symbol
from_udefn :: UDefn -> USymb
from_udefn (USynonym x) = x
from_udefn (UScale _ s) = s
from_udefn (UShift _ s) = s

