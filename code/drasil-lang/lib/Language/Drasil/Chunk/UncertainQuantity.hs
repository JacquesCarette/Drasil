{-# Language TemplateHaskell #-}
-- | For adding an uncertainty value to quantities with constraints.
module Language.Drasil.Chunk.UncertainQuantity (
  -- * Chunk Types
  UncertQ,
  -- * Constructors
  uq, uqc,
  uqcND) where

import Control.Lens ((^.), makeLenses, view)

import Drasil.Database (HasUID(..), HasChunkRefs(..))

import Language.Drasil.Chunk.DefinedQuantity (dqdWr)
import Language.Drasil.Chunk.Constrained (ConstrConcept(..), cuc')
import Language.Drasil.Symbol
import Language.Drasil.Classes (NamedIdea(term), Idea(getA), Express(express),
  Definition(defn), ConceptDomain(cdom), Concept, Quantity,
  IsUnit, Constrained(constraints), HasReasVal(reasVal))
import Language.Drasil.Constraint (ConstraintE)
import Language.Drasil.Chunk.UnitDefn (MayHaveUnit(getUnit))
import Language.Drasil.Expr.Lang (Expr)
import Language.Drasil.Expr.Class (sy)
import Language.Drasil.NounPhrase.Types (NP)
import Language.Drasil.Space (Space, HasSpace(..))
import Language.Drasil.Uncertainty

-- | UncertQs are conceptual symbolic quantities with constraints and an 'Uncertainty'.
-- Contains a 'ConstrConcept' and an 'Uncertainty'.
--
-- Ex. Measuring the length of a pendulum arm may be recorded with an uncertainty value.
data UncertQ = UQ { _coco :: ConstrConcept , _unc'' :: Uncertainty }
makeLenses ''UncertQ

-- | Equal if 'UID's are equal.
instance Eq             UncertQ where a == b = (a ^. uid) == (b ^. uid)
-- | Finds 'UID' of the 'ConstrConcept' used to make the 'UncertQ'.
instance HasUID         UncertQ where uid = coco . uid
instance HasChunkRefs   UncertQ where
  chunkRefs = const mempty -- FIXME: `chunkRefs` should actually collect the referenced chunks.
-- | Finds term ('NP') of the 'ConstrConcept' used to make the 'UncertQ'.
instance NamedIdea      UncertQ where term = coco . term
-- | Finds the idea contained in the 'ConstrConcept' used to make the 'UncertQ'.
instance Idea           UncertQ where getA (UQ q _) = getA q
-- | Finds the 'Space' of the 'ConstrConcept' used to make the 'UncertQ'.
instance HasSpace       UncertQ where typ = coco . typ
-- | Finds the 'Symbol' of the 'ConstrConcept' used to make the 'UncertQ'.
instance HasSymbol      UncertQ where symbol c = symbol (c^.coco)
-- | 'UncertQ's have a 'Quantity'.
instance Quantity       UncertQ where
-- | Finds the uncertainty of an 'UncertQ'.
instance HasUncertainty UncertQ where unc = unc''
-- | Finds the 'Constraint's of a 'ConstrConcept' used to make the 'UncertQ'.
instance Constrained    UncertQ where constraints = coco . constraints
-- | Finds a reasonable value for the 'ConstrConcept' used to make the 'UncertQ'.
instance HasReasVal     UncertQ where reasVal = coco . reasVal
-- | Finds definition of the 'ConstrConcept' used to make the 'UncertQ'.
instance Definition     UncertQ where defn = coco . defn
-- | Finds the domain contained in the 'ConstrConcept' used to make the 'UncertQ'.
instance ConceptDomain  UncertQ where cdom = cdom . view coco
-- | Finds the units of the 'ConstrConcept' used to make the 'UncertQ'.
instance MayHaveUnit    UncertQ where getUnit = getUnit . view coco
-- | Convert the symbol of the 'UncertQ' to a 'ModelExpr'.
instance Express        UncertQ where express = sy

{-- Constructors --}
-- | Smart constructor that requires a 'Quantity', a percentage, and a typical value with an 'Uncertainty'.
uq :: (Quantity c, Constrained c, Concept c, HasReasVal c, MayHaveUnit c) =>
  c -> Uncertainty -> UncertQ
uq q = UQ (ConstrConcept (dqdWr q) (q ^. constraints) (q ^. reasVal))

--FIXME: this is kind of crazy and probably shouldn't be used!
-- | Uncertainty quantity ('uq') but with a constraint.
uqc :: (IsUnit u) => String -> NP -> String -> Symbol -> u -> Space
                -> [ConstraintE] -> Expr -> Uncertainty -> UncertQ
uqc nam trm desc sym un space cs val = uq (cuc' nam trm desc sym un space cs val)

-- | Uncertainty quantity constraint ('uqc') without a description.
uqcND :: (IsUnit u) => String -> NP -> Symbol -> u -> Space -> [ConstraintE]
                  -> Expr -> Uncertainty -> UncertQ
uqcND nam trm sym un space cs val = uq (cuc' nam trm "" sym un space cs val)
