{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs, Rank2Types #-}
module Unit (
    USymb(..), UDefn(..)        -- languages
  , Unit(..), UnitEq(..)        -- classes
  , FundUnit(..), DerUChunk(..) -- data-structures
  , UnitDefn(..)                -- wrapper for 'Unit' class
  , from_udefn
  , makeDerU
  , unitCon
  ) where

import Chunk (ConceptChunk(..), Chunk(..), Concept(..))
import Symbol (Symbol)
import Control.Lens (Simple, Lens, set, (^.))

-- Language of units (how to build them up)
-- UName for the base cases, otherwise build up.
-- Probably a 7-vector would be better (less error-prone!)
data USymb = UName Symbol
           | UProd [USymb]
           | UPow USymb Integer -- can be negative, should not be 0
           | UDiv USymb USymb   --Get proper division (not using negative powers)
                                --  necessary for things like J/(kg*C)
-- Language of unit equations, to define a unit relative
-- to another
data UDefn = USynonym USymb      -- to define straight synonyms
           | UScale Double USymb -- scale, i.e. *
           | UShift Double USymb -- shift, i.e. +

class Concept u => Unit u where
   unit :: Simple Lens u USymb

class UnitEq u where
   uniteq :: Simple Lens u UDefn

-- Can generate a default symbol
from_udefn :: UDefn -> USymb
from_udefn (USynonym x) = x
from_udefn (UScale _ s) = s
from_udefn (UShift _ s) = s

makeDerU :: ConceptChunk -> UDefn -> DerUChunk
makeDerU concept eqn = DUC (UD concept (from_udefn eqn)) eqn

unitCon :: String -> ConceptChunk
unitCon s = CC s s
---------------------------------------------------------

-- for defining fundamental units
data FundUnit = UD { _vc :: ConceptChunk, _u :: USymb }

-- don't export this
vc :: Simple Lens FundUnit ConceptChunk
vc f (UD a b) = fmap (\x -> UD x b) (f a)

instance Chunk FundUnit where
  name = vc . name

instance Concept FundUnit where
  descr = vc . descr

instance Unit FundUnit where
  unit f (UD a b) = fmap (\x -> UD a x) (f b)

-- and for defining Derived units
data DerUChunk = DUC { _uc :: FundUnit, _eq :: UDefn }

-- don't export this either
duc :: Simple Lens DerUChunk FundUnit
duc f (DUC a b) = fmap (\x -> DUC x b) (f a)

instance Chunk   DerUChunk where name  = duc . name
instance Concept DerUChunk where descr = duc . descr
instance Unit    DerUChunk where unit  = duc . unit

instance UnitEq DerUChunk where
  uniteq f (DUC a b) = fmap (\x -> DUC a x) (f b)

----------------------------------------------------------

-- For allowing lists to mix the two, thus forgetting
-- the definition part

data UnitDefn where
  UU :: Unit u => u -> UnitDefn

-- don't export
ulens :: (forall u. Unit u => Simple Lens u a) -> Simple Lens UnitDefn a
ulens l f (UU a) = fmap (\x -> UU (set l x a)) (f (a ^. l))

instance Unit    UnitDefn where unit  = ulens unit
instance Chunk   UnitDefn where name  = ulens name
instance Concept UnitDefn where descr = ulens descr
