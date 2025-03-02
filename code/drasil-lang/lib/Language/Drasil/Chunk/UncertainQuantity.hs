{-# Language TemplateHaskell #-}
-- | For adding an uncertainty value to quantities with constraints.
module Language.Drasil.Chunk.UncertainQuantity (
  -- * Chunk Types
  UncertQ, UncertainChunk(..),
  -- * Constructors
  uq, uqc,
  uqcND, uncrtnChunk, uvc, uncrtnw) where
 
import Language.Drasil.Chunk.DefinedQuantity (dqdWr)
import Language.Drasil.Chunk.Constrained (ConstrConcept(..), ConstrainedChunk, cuc', cnstrw, cvc)
import Language.Drasil.Symbol
import Language.Drasil.Classes (NamedIdea(term), Idea(getA), Express(express),
  Definition(defn), ConceptDomain(cdom), Concept, Quantity,
  IsUnit, Constrained(constraints), HasReasVal(reasVal))
import Language.Drasil.Constraint (ConstraintE)
import Language.Drasil.Chunk.UnitDefn (MayHaveUnit(getUnit))
import Language.Drasil.Expr.Lang (Expr)
import Language.Drasil.Expr.Class (sy)
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Space (Space, HasSpace(..))
import Language.Drasil.Uncertainty
import Drasil.Database.UID (HasUID(..))

import Control.Lens ((^.), makeLenses, view)

{- The order of the following two implementations is the same as in Constrained -}

-- | UncertainChunk is a symbolic quantity with constraints, a typical value, and an uncertainty. 
-- Contains a 'ConstrainedChunk' and an 'Uncertainty'.
--
-- Ex. Measuring the length of a pendulum arm may be recorded with an uncertainty value.
data UncertainChunk  = UCh { _conc :: ConstrainedChunk , _unc' :: Uncertainty }
makeLenses ''UncertainChunk

-- | Finds 'UID' of the 'ConstrainedChunk' used to make the 'UncertainChunk'.
instance HasUID            UncertainChunk where uid = conc . uid
-- | Equal if 'UID's are equal.
instance Eq                UncertainChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds term ('NP') of the 'ConstrainedChunk' used to make the 'UncertainChunk'.
instance NamedIdea         UncertainChunk where term = conc . term
-- | Finds the idea contained in the 'ConstrainedChunk' used to make the 'UncertainChunk'.
instance Idea              UncertainChunk where getA (UCh n _) = getA n
-- | Finds the 'Space' of the 'ConstrainedChunk' used to make the 'UncertainChunk'.
instance HasSpace          UncertainChunk where typ = conc . typ
-- | Finds the 'Symbol' of the 'ConstrainedChunk' used to make the 'UncertainChunk'.
instance HasSymbol         UncertainChunk where symbol c = symbol (c^.conc)
-- | 'UncertainChunk's have a 'Quantity'.
instance Quantity          UncertainChunk where
-- | Finds the 'Constraint's of the 'ConstrainedChunk' used to make the 'UncertainChunk'.
instance Constrained       UncertainChunk where constraints = conc . constraints
-- | Finds a reasonable value for the 'ConstrainedChunk' used to make the 'UncertainChunk'.
instance HasReasVal        UncertainChunk where reasVal = conc . reasVal
-- | Finds the uncertainty of an 'UncertainChunk'.
instance HasUncertainty    UncertainChunk where unc = unc'
-- | Finds units contained in the 'ConstrainedChunk' used to make the 'UncertainChunk'.
instance MayHaveUnit       UncertainChunk where getUnit = getUnit . view conc

{-- Constructors --}
-- | Smart constructor that can project to an 'UncertainChunk' (also given an 'Uncertainty').
uncrtnChunk :: (Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => 
  c -> Uncertainty -> UncertainChunk
uncrtnChunk q = UCh (cnstrw q)

-- | Creates an uncertain variable chunk. Takes 'UID', term ('NP'),
-- 'Symbol', 'Space', 'Constrains', 'Expr', and 'Uncertainty'.
uvc :: String -> NP -> Symbol -> Space -> [ConstraintE] -> Expr -> Uncertainty -> UncertainChunk
uvc nam trm sym space cs val = uncrtnChunk (cvc nam trm sym space cs (Just val))

-- | Projection function into an 'UncertainChunk' from 'UncertQ' or an 'UncertainChunk'.
uncrtnw :: (HasUncertainty c, Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => c -> UncertainChunk
uncrtnw c = UCh (cnstrw c) (c ^. unc)

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
