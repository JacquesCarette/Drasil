{-# Language TemplateHaskell #-}
-- | Add constraints and a reasonable value to chunks that are quantities.
module Language.Drasil.Chunk.Constrained (
  -- * Constrained Chunks
  -- ** From an Idea
  Constrain(..), cuc, cvc, cnstrw,
  -- ** From a Concept
  ConstrConcept(..),
  cnstrw', constrained', constrainedNRV', cuc', cuc'') where

import Control.Lens ((^.), makeLenses, view)

import Language.Drasil.Chunk.Concept (cw, dcc)
import Language.Drasil.Chunk.DefinedQuantity (DefinedQuantityDict, dqd, dqd', dqdWr)
import Language.Drasil.Chunk.Quantity (QuantityDict, qw, vc)
import Language.Drasil.Chunk.Unital (uc')
import Language.Drasil.Chunk.Unitary (unitary)
import Language.Drasil.Symbol (HasSymbol(..), Symbol)
import Language.Drasil.Classes (NamedIdea(term), Idea(getA), Express(express),
  Definition(defn), ConceptDomain(cdom), Concept, Quantity,
  IsUnit, Constrained(constraints), HasReasVal(reasVal))
import Language.Drasil.Constraint (ConstraintE)
import Language.Drasil.Chunk.UnitDefn (unitWrapper, MayHaveUnit(getUnit))
import Language.Drasil.Expr.Lang (Expr(..))
import Language.Drasil.Expr.Class (sy)
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Sentence (Sentence(S))
import Language.Drasil.Space (Space, HasSpace(..))
import Language.Drasil.Stages (Stage)
import Language.Drasil.UID (HasUID(..))

-- | Constrains are symbolic quantities ('QuantityDict')
-- with 'Constraint's and maybe a typical value ('Maybe' 'Expr').
--
-- Ex. Measuring the length of a pendulum would have some reasonable value (between 1 cm and 2 m)
-- and the constraint that the length cannot be a negative value.
data Constrain = Constrain { _qd     :: QuantityDict
                                         , _constr :: [ConstraintE]
                                         , _reasV  :: Maybe Expr
                                         }
makeLenses ''Constrain

-- | Finds 'UID' of the 'QuantityDict' used to make the 'Constrain'.
instance HasUID        Constrain where uid = qd . uid
-- | Finds term ('NP') of the 'QuantityDict' used to make the 'Constrain'.
instance NamedIdea     Constrain where term = qd . term
-- | Finds the idea contained in the 'QuantityDict' used to make the 'Constrain'.
instance Idea          Constrain where getA = getA . view qd
-- | Finds the 'Space' of the 'QuantityDict' used to make the 'Constrain'.
instance HasSpace      Constrain where typ = qd . typ
-- | Finds the 'Symbol' of the 'QuantityDict' used to make the 'Constrain'.
instance HasSymbol     Constrain where symbol c = symbol (c^.qd)
-- | 'Constrain's have a 'Quantity'. 
instance Quantity      Constrain where
-- | Finds the 'Constraint's of a 'Constrain'.
instance Constrained   Constrain where constraints = constr
-- | Finds a reasonable value for the 'Constrain'.
instance HasReasVal    Constrain where reasVal     = reasV
-- | Equal if 'UID's are equal.
instance Eq            Constrain where c1 == c2 = (c1 ^. qd . uid) == (c2 ^. qd . uid)
-- | Finds units contained in the 'QuantityDict' used to make the 'Constrain'.
instance MayHaveUnit   Constrain where getUnit = getUnit . view qd

-- | Creates a constrained unitary chunk from a 'UID', term ('NP'), 'Symbol', unit, 'Space', 'Constraint's, and an 'Expr'.
cuc :: (IsUnit u) => String -> NP -> Symbol -> u
  -> Space -> [ConstraintE] -> Expr -> Constrain
cuc i t s u space cs rv = Constrain (qw (unitary i t s u space)) cs (Just rv)

-- | Creates a constrained unitary chunk from a 'UID', term ('NP'), 'Symbol', 'Space', 'Constraint's, and a 'Maybe' 'Expr' (Similar to 'cuc' but no units).
cvc :: String -> NP -> Symbol -> Space -> [ConstraintE] -> Maybe Expr -> Constrain
cvc i des sym space = Constrain (qw (vc i des sym space))

-- | Creates a new Constrain from either a 'Constrain', 'ConstrConcept', 'UncertainChunk', or an 'UncertQ'.
cnstrw :: (Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => c -> Constrain
cnstrw c = Constrain (qw c) (c ^. constraints) (c ^. reasVal)

-- | ConstrConcepts are conceptual symbolic quantities ('DefinedQuantityDict')
-- with 'Constraint's and maybe a reasonable value (no units!).
-- Similar to 'Constrain' but includes a definition and domain. 
--
-- Ex. Measuring the length of a pendulum arm could be a concept that has some reasonable value
-- (between 1 cm and 2 m) and the constraint that the length cannot be a negative value.
data ConstrConcept = ConstrConcept { _defq    :: DefinedQuantityDict
                                   , _constr' :: [ConstraintE]
                                   , _reasV'  :: Maybe Expr
                                   }
makeLenses ''ConstrConcept

-- | Finds 'UID' of the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance HasUID        ConstrConcept where uid = defq . uid
-- | Finds term ('NP') of the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance NamedIdea     ConstrConcept where term = defq . term
-- | Finds the idea contained in the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance Idea          ConstrConcept where getA = getA . view defq
-- | Finds the 'Space' of the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance HasSpace      ConstrConcept where typ = defq . typ
-- | Finds the 'Symbol' of the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance HasSymbol     ConstrConcept where symbol c = symbol (c^.defq)
-- | 'ConstrConcept's have a 'Quantity'. 
instance Quantity      ConstrConcept where
-- | Finds definition of the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance Definition    ConstrConcept where defn = defq . defn
-- | Finds the domain contained in the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance ConceptDomain ConstrConcept where cdom = cdom . view defq
-- | Finds the 'Constraint's of a 'ConstrConcept'.
instance Constrained   ConstrConcept where constraints  = constr'
-- | Finds a reasonable value for the 'ConstrConcept'.
instance HasReasVal    ConstrConcept where reasVal      = reasV'
-- | Equal if 'UID's are equal.
instance Eq            ConstrConcept where c1 == c2 = (c1 ^.defq.uid) == (c2 ^.defq.uid)
-- | Finds the units of the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance MayHaveUnit   ConstrConcept where getUnit = getUnit . view defq
-- | Convert the symbol of the 'ConstrConcept' to a 'ModelExpr'.
instance Express       ConstrConcept where express = sy

-- | Creates a 'ConstrConcept' with a quantitative concept, a list of 'Constraint's and an 'Expr'.
constrained' :: (Concept c, MayHaveUnit c, Quantity c) =>
  c -> [ConstraintE] -> Expr -> ConstrConcept
constrained' q cs rv = ConstrConcept (dqdWr q) cs (Just rv)

-- | Similar to 'constrained'', but defaults 'Maybe' 'Expr' to 'Nothing'.
constrainedNRV' :: (Concept c, MayHaveUnit c, Quantity c) =>
  c -> [ConstraintE] -> ConstrConcept
constrainedNRV' q cs = ConstrConcept (dqdWr q) cs Nothing

-- | Creates a constrained unitary chunk from a 'UID', term ('NP'), description ('String'), 'Symbol', unit, 'Space', 'Constraint's, and an 'Expr'.
cuc' :: (IsUnit u) => String -> NP -> String -> Symbol -> u
            -> Space -> [ConstraintE] -> Expr -> ConstrConcept
cuc' nam trm desc sym un space cs rv =
  ConstrConcept (dqd (cw (uc' nam trm (S desc) sym space un)) sym space uu) cs (Just rv)
  where uu = unitWrapper un

-- | Similar to 'cuc'', but 'Symbol' is dependent on 'Stage'.
cuc'' :: (IsUnit u) => String -> NP -> String -> (Stage -> Symbol) -> u
            -> Space -> [ConstraintE] -> Expr -> ConstrConcept
cuc'' nam trm desc sym un space cs rv =
  ConstrConcept (dqd' (dcc nam trm desc) sym space (Just uu)) cs (Just rv)
  where uu = unitWrapper un

-- | Similar to 'cnstrw', but types must also have a 'Concept'.
cnstrw' :: (Quantity c, Concept c, Constrained c, HasReasVal c, MayHaveUnit c) => c -> ConstrConcept
cnstrw' c = ConstrConcept (dqdWr c) (c ^. constraints) (c ^. reasVal)
