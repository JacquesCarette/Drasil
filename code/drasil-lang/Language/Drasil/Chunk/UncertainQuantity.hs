{-# Language TemplateHaskell #-}

module Language.Drasil.Chunk.UncertainQuantity 
  ( UncertQ, UncertainChunk(..) , uq, uqc
  , uqcND, uncrtnChunk, uvc, uncrtnw) where
 
import Language.Drasil.Chunk.DefinedQuantity (dqdWr)
import Language.Drasil.Chunk.Constrained (ConstrConcept(..), ConstrReasQDef, cuc', cnstrw, constrReasQD)
import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, Quantity, HasSpace(typ),
  IsUnit, Constrained(constraints), HasReasVal(reasVal), MayHaveReasVal(..), HasUncertainty (unc))
import Language.Drasil.Constraint (Constraint)
import Language.Drasil.Chunk.UnitDefn (MayHaveUnit(getUnit))
import Language.Drasil.Expr (Expr)
import Language.Drasil.NounPhrase(NP)
import Language.Drasil.Space (Space)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Uncertainty.Core (Uncertainty)
import Control.Lens ((^.), makeLenses, view)

{- The order of the following two implementations is the same as in Constrained -}

-- | UncertainChunk is a symbolic quantity with constraints, a typical value, and an uncertainty. 
-- Contains a 'ConstrainedChunk' and an 'Uncertainty'.
data UncertainChunk  = UCh { _conc :: ConstrReasQDef , _unc' :: Uncertainty }
makeLenses ''UncertainChunk

instance HasUID            UncertainChunk where uid = conc . uid
-- ^ Finds 'UID' of the 'ConstrainedChunk' used to make the 'UncertainChunk'.
instance Eq                UncertainChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- ^ Equal if 'UID's are equal.
instance NamedIdea         UncertainChunk where term = conc . term
-- ^ Finds term ('NP') of the 'ConstrainedChunk' used to make the 'UncertainChunk'.
instance Idea              UncertainChunk where getA (UCh n _) = getA n
-- ^ Finds the idea contained in the 'ConstrainedChunk' used to make the 'UncertainChunk'.
instance HasSpace          UncertainChunk where typ = conc . typ
-- ^ Finds the 'Space' of the 'ConstrainedChunk' used to make the 'UncertainChunk'.
instance HasSymbol         UncertainChunk where symbol c = symbol (c^.conc)
-- ^ Finds the 'Symbol' of the 'ConstrainedChunk' used to make the 'UncertainChunk'.
instance Quantity          UncertainChunk where
-- ^ 'UncertainChunk's have a 'Quantity'.
instance Constrained       UncertainChunk where constraints = conc . constraints
-- ^ Finds the 'Constraint's of the 'ConstrainedChunk' used to make the 'UncertainChunk'.
instance HasReasVal        UncertainChunk where reasVal = conc . reasVal
-- ^ Finds a reasonable value for the 'ConstrainedChunk' used to make the 'UncertainChunk'.
instance HasUncertainty    UncertainChunk where unc = unc'
-- ^ Finds the uncertainty of an 'UncertainChunk'.
instance MayHaveUnit       UncertainChunk where getUnit = getUnit . view conc
-- ^ Finds units contained in the 'ConstrainedChunk' used to make the 'UncertainChunk'.

{-- Constructors --}
-- | Smart constructor that can project to an 'UncertainChunk' (also given an 'Uncertainty').
uncrtnChunk :: (Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => 
  c -> Uncertainty -> UncertainChunk
uncrtnChunk q = UCh (cnstrw q)

-- | Creates an uncertain variable chunk. Takes 'UID', term ('NP'),
-- 'Symbol', 'Space', 'Constraints', 'Expr', and 'Uncertainty'.
uvc :: String -> NP -> Symbol -> Space -> [Constraint] -> Expr -> Uncertainty -> UncertainChunk
uvc nam trm sym space cs val = uncrtnChunk (constrReasQD nam trm sym space cs val)

-- | Projection function into an 'UncertainChunk' from 'UncertQ' or an 'UncertainChunk'.
uncrtnw :: (HasUncertainty c, Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => c -> UncertainChunk
uncrtnw c = UCh (cnstrw c) (c ^. unc)

-- | UncertQs are conceptual symbolic quantities with constraints and an 'Uncertainty'.
-- Contains a 'ConstrConcept' and an 'Uncertainty'.
data UncertQ = UQ { _coco :: ConstrConcept , _unc'' :: Uncertainty }
makeLenses ''UncertQ
  
instance Eq             UncertQ where a == b = (a ^. uid) == (b ^. uid)
-- ^ Equal if 'UID's are equal.
instance HasUID         UncertQ where uid = coco . uid
-- ^ Finds 'UID' of the 'ConstrConcept' used to make the 'UncertQ'.
instance NamedIdea      UncertQ where term = coco . term
-- ^ Finds term ('NP') of the 'ConstrConcept' used to make the 'UncertQ'.
instance Idea           UncertQ where getA (UQ q _) = getA q
-- ^ Finds the idea contained in the 'ConstrConcept' used to make the 'UncertQ'.
instance HasSpace       UncertQ where typ = coco . typ
-- ^ Finds the 'Space' of the 'ConstrConcept' used to make the 'UncertQ'.
instance HasSymbol      UncertQ where symbol c = symbol (c^.coco)
-- ^ Finds the 'Symbol' of the 'ConstrConcept' used to make the 'UncertQ'.
instance Quantity       UncertQ where 
-- ^ 'UncertQ's have a 'Quantity'.
instance HasUncertainty UncertQ where unc = unc''
-- ^ Finds the uncertainty of an 'UncertQ'.
instance Constrained    UncertQ where constraints = coco . constraints
-- ^ Finds the 'Constraint's of a 'ConstrConcept' used to make the 'UncertQ'.
instance MayHaveReasVal     UncertQ where maybeReasVal = coco . maybeReasVal
-- ^ Finds a reasonable value for the 'ConstrConcept' used to make the 'UncertQ'.
instance Definition     UncertQ where defn = coco . defn
-- ^ Finds definition of the 'ConstrConcept' used to make the 'UncertQ'.
instance ConceptDomain  UncertQ where cdom = cdom . view coco
-- ^ Finds the domain contained in the 'ConstrConcept' used to make the 'UncertQ'.
instance MayHaveUnit    UncertQ where getUnit = getUnit . view coco
-- ^ Finds the units of the 'ConstrConcept' used to make the 'UncertQ'.

{-- Constructors --}
-- | Smart constructor that requires a 'Quantity', a percentage, and a typical value with an 'Uncertainty'.
uq :: (Quantity c, Constrained c, Concept c, MayHaveReasVal c, MayHaveUnit c) =>
  c -> Uncertainty -> UncertQ
uq q = UQ (ConstrConcept (dqdWr q) (q ^. constraints) (q ^. maybeReasVal))

--FIXME: this is kind of crazy and probably shouldn't be used!
-- | Uncertainty quantity ('uq') but with a constraint.
uqc :: (IsUnit u) => String -> NP -> String -> Symbol -> u -> Space
                -> [Constraint] -> Expr -> Uncertainty -> UncertQ
uqc nam trm desc sym un space cs val = uq (cuc' nam trm desc sym un space cs val)

-- | Uncertainty quantity constraint ('uqc') without a description.
uqcND :: (IsUnit u) => String -> NP -> Symbol -> u -> Space -> [Constraint]
                  -> Expr -> Uncertainty -> UncertQ
uqcND nam trm sym un space cs val = uq (cuc' nam trm "" sym un space cs val)
