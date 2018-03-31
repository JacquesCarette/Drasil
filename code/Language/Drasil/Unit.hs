module Language.Drasil.Unit (
    IsUnit, UnitEq(..),HasUnitSymbol(..)        -- classes
  , FundUnit(..), DerUChunk(..) -- data-structures
  , UnitDefn                    -- synonym for FundUnit
  , from_udefn, makeDerU, unitCon
  , (^:), (/:), (*:), new_unit
  , scale, shift
  , derUC, derUC', derUC'', unitWrapper
  ) where

import Control.Lens (Simple, Lens, (^.))

import Language.Drasil.Chunk (Chunk(..))
import Language.Drasil.Chunk.NamedIdea (NamedIdea(..), Idea(..))
import Language.Drasil.Chunk.Concept (Concept,Definition(..), 
  ConceptDomain(..),ConceptChunk, dcc, cw)
import Language.Drasil.Symbol
import Language.Drasil.UnitLang

import Language.Drasil.NounPhrase (cn,cn',NP)

-- | Some chunks store a unit symbol
class HasUnitSymbol u where
   usymb :: Simple Lens u USymb

-- | Units are concepts which store a unit symbol.
-- They must also be explicitly declared to be instances of IsUnit
class (Concept u, HasUnitSymbol u) => IsUnit u where

class UnitEq u where
   uniteq :: Simple Lens u UDefn

-- | Create a derived unit chunk from a concept and a unit equation
makeDerU :: ConceptChunk -> UDefn -> DerUChunk
makeDerU concept eqn = DUC (UD concept (from_udefn eqn)) eqn

-- | Create a derived unit chunk from an id, term (as 'String'), definition,
-- symbol, and unit equation
derUC, derUC' :: String -> String -> String -> Symbol -> UDefn -> DerUChunk
-- | Uses self-plural term
derUC  a b c s u = DUC (UD (dcc a (cn b) c) (UName $ s)) u
-- | Uses term that pluralizes by adding *s* to the end
derUC' a b c s u = DUC (UD (dcc a (cn' b) c) (UName $ s)) u

-- | Create a derived unit chunk from an id, term (as noun phrase), definition, 
-- symbol, and unit equation
derUC'' :: String -> NP -> String -> Symbol -> UDefn -> DerUChunk
derUC'' a b c s u = DUC (UD (dcc a b c) (UName $ s)) u

--FIXME: Make this use a meaningful identifier.
-- | Helper for fundamental unit concept chunk creation. Uses the same string
-- for the identifier, term, and definition.
unitCon :: String -> ConceptChunk
unitCon s = dcc s (cn' s) s
---------------------------------------------------------

-- | for defining fundamental units
data FundUnit = UD { _vc :: ConceptChunk, _u :: USymb }

-- don't export this
vc :: Simple Lens FundUnit ConceptChunk
vc f (UD a b) = fmap (\x -> UD x b) (f a)

instance Chunk         FundUnit where uid = vc . uid
instance NamedIdea     FundUnit where term   = vc . term
instance Idea          FundUnit where getA c = getA (c ^. vc)
instance Definition    FundUnit where defn = vc . defn
instance ConceptDomain FundUnit where cdom = vc . cdom
instance Concept       FundUnit where
instance HasUnitSymbol FundUnit where usymb f (UD a b) = fmap (\x -> UD a x) (f b)
instance IsUnit        FundUnit

-- | for defining Derived units
data DerUChunk = DUC { _uc :: FundUnit, _eq :: UDefn }

-- don't export this either
duc :: Simple Lens DerUChunk FundUnit
duc f (DUC a b) = fmap (\x -> DUC x b) (f a)

instance Chunk         DerUChunk where uid  = duc . uid
instance NamedIdea     DerUChunk where term = duc . term
instance Idea          DerUChunk where getA c = getA (c ^. duc)
instance Definition    DerUChunk where defn = duc . defn
instance ConceptDomain DerUChunk where cdom = duc . cdom
instance Concept       DerUChunk where
instance HasUnitSymbol DerUChunk where usymb  = duc . usymb
instance IsUnit        DerUChunk where

instance UnitEq DerUChunk where
  uniteq f (DUC a b) = fmap (\x -> DUC a x) (f b)

----------------------------------------------------------

-- | For allowing lists to mix the two, thus forgetting
-- the definition part
unitWrapper :: IsUnit u => u -> FundUnit
unitWrapper u = UD (cw u) (u ^. usymb)

--- These conveniences go here, because we need the class
-- | Combinator for raising a unit to a power
(^:) :: IsUnit u => u -> Integer -> USymb
u ^: i = UPow (u ^. usymb) i

-- | Combinator for dividing one unit by another
(/:) :: (IsUnit u1, IsUnit u2) => u1 -> u2 -> USymb
u1 /: u2 = UDiv (u1 ^. usymb) (u2 ^. usymb)

-- | Combinator for multiplying two units together
(*:) :: (IsUnit u1, IsUnit u2) => u1 -> u2 -> USymb
u1 *: u2 = UProd [(u1 ^. usymb), (u2 ^. usymb)]

-- | Combinator for scaling one unit by some number
scale :: IsUnit s => Double -> s -> UDefn
scale a b = UScale a (b ^. usymb)

-- | Combinator for shifting one unit by some number
shift :: IsUnit s => Double -> s -> UDefn
shift a b = UShift a (b ^. usymb)

-- | Smart constructor for new derived units from existing units.
new_unit :: String -> USymb -> DerUChunk
new_unit s u = makeDerU (unitCon s) (USynonym u)

type UnitDefn = FundUnit

instance Eq FundUnit where a == b = (a ^. usymb) == (b ^. usymb)
instance Ord FundUnit where compare a b = compare (a ^. usymb) (b ^. usymb)
