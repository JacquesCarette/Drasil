{-# Language TemplateHaskell #-}
-- | For adding an uncertainty value to quantities with constraints.
module Language.Drasil.Chunk.UncertainQuantity (
  -- * Chunk Types
  UncertQ,
  -- * Constructors
  uq, uqc,
  uqcND,
  -- * Direct constructor (preserves ConstrConcept fields)
  uqDirect) where

import Control.Lens ((^.), makeLenses, view)

import Drasil.Database (HasUID(..), HasChunkRefs(..), UID)

import Language.Drasil.Chunk.DefinedQuantity (DefinedQuantityDict, dqdWr)
import Language.Drasil.Chunk.Constrained (ConstrConcept(..), cuc')
import Language.Drasil.Symbol
import Language.Drasil.Classes (NamedIdea(term), Idea(getA), Express(express),
  Definition(defn), Concept, Quantity,
  Constrained(constraints), HasReasVal(reasVal), MayHaveRationale(rationale))
import Language.Drasil.Constraint (ConstraintE)
import Language.Drasil.Chunk.UnitDefn (MayHaveUnit(getUnit), UnitDefn)
import Language.Drasil.Expr.Lang (Expr)
import Language.Drasil.Expr.Class (sy)
import Language.Drasil.NaturalLanguage.English.NounPhrase.Core (NP)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.Space (Space, HasSpace(..))
import Language.Drasil.Uncertainty

-- | UncertQs are conceptual symbolic quantities with constraints and an 'Uncertainty'.
-- Contains the same information as a 'ConstrConcept' with an added 'Uncertainty'.
--
-- Ex. Measuring the length of a pendulum arm may be recorded with an uncertainty value.
data UncertQ = UQ { _uu         :: UID
                  , _defq       :: DefinedQuantityDict
                  , _constr'    :: [ConstraintE]
                  , _reasV'     :: Maybe Expr
                  , _rationale' :: Maybe Sentence
                  , _unc''      :: Uncertainty
                  }
makeLenses ''UncertQ

instance HasChunkRefs UncertQ where
  chunkRefs c = chunkRefs (c ^. defq)
  {-# INLINABLE chunkRefs #-}

-- | Equal if 'UID's are equal.
instance Eq             UncertQ where a == b = (a ^. uid) == (b ^. uid)
-- | Finds 'UID' of the 'DefinedQuantityDict' used to make the 'UncertQ'.
instance HasUID         UncertQ where uid = uu
-- | Finds term ('NP') of the 'DefinedQuantityDict' used to make the 'UncertQ'.
instance NamedIdea      UncertQ where term = defq . term
-- | Finds the idea contained in the 'DefinedQuantityDict' used to make the 'UncertQ'.
instance Idea           UncertQ where getA = getA . view defq
-- | Finds the 'Space' of the 'DefinedQuantityDict' used to make the 'UncertQ'.
instance HasSpace       UncertQ where typ = defq . typ
-- | Finds the 'Symbol' of the 'DefinedQuantityDict' used to make the 'UncertQ'.
instance HasSymbol      UncertQ where symbol c = symbol (c ^. defq)
-- | Finds the uncertainty of an 'UncertQ'.
instance HasUncertainty UncertQ where unc = unc''
-- | Finds the 'Constraint's of a 'UncertQ'.
instance Constrained    UncertQ where constraints = constr'
-- | Finds a reasonable value for the 'UncertQ'.
instance HasReasVal     UncertQ where reasVal = reasV'
-- | Finds the rationale for the 'UncertQ'.
instance MayHaveRationale   UncertQ where rationale = rationale'
-- | Finds definition of the 'DefinedQuantityDict' used to make the 'UncertQ'.
instance Definition     UncertQ where defn = defq . defn
-- | Finds the units of the 'DefinedQuantityDict' used to make the 'UncertQ'.
instance MayHaveUnit    UncertQ where getUnit = getUnit . view defq
-- | Convert the symbol of the 'UncertQ' to a 'ModelExpr'.
instance Express        UncertQ where express = sy

{-- Constructors --}
-- | Smart constructor that requires a 'Quantity', a percentage, and a reasonable value with an 'Uncertainty'.
uq :: (Quantity c, Constrained c, Concept c, HasReasVal c, MayHaveUnit c) =>
  c -> Uncertainty -> UncertQ
uq q = UQ (q ^. uid) (dqdWr q) (q ^. constraints) (q ^. reasVal) Nothing

--FIXME: this is kind of crazy and probably shouldn't be used!
-- | Uncertainty quantity ('uq') but with a constraint.
uqc :: String -> NP -> String -> Symbol -> UnitDefn -> Space
                -> [ConstraintE] -> Expr -> Uncertainty -> UncertQ
uqc nam trm desc sym un space cs val = uq (cuc' nam trm desc sym un space cs val)

-- | Uncertainty quantity constraint ('uqc') without a description.
uqcND :: String -> NP -> Symbol -> UnitDefn -> Space -> [ConstraintE]
                  -> Expr -> Uncertainty -> UncertQ
uqcND nam trm sym un space cs val = uq (cuc' nam trm "" sym un space cs val)

-- | Directly wraps a 'ConstrConcept' with an 'Uncertainty', preserving all fields (including rationale).
uqDirect :: ConstrConcept -> Uncertainty -> UncertQ
uqDirect c = UQ (c ^. uid) (dqdWr c) (c ^. constraints) (c ^. reasVal) (c ^. rationale)
