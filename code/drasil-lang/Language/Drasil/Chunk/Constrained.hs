{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.Constrained (ConstrainedChunk(..), ConstrConcept(..),
  cnstrw, cnstrw', constrained', constrainedNRV', cuc, cuc', cuc'', cvc) where

import Control.Lens ((^.), makeLenses, view)

import Language.Drasil.Chunk.Concept (cw, dcc)
import Language.Drasil.Chunk.DefinedQuantity (DefinedQuantityDict, dqd, dqd', dqdWr)
import Language.Drasil.Chunk.Quantity (QuantityDict, qw, vc)
import Language.Drasil.Chunk.Unital (ucs)
import Language.Drasil.Chunk.Unitary (unitary)
import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, Quantity, HasSpace(typ),
  IsUnit, Constrained(constraints), HasReasVal(reasVal))
import Language.Drasil.Constraint (Constraint(..))
import Language.Drasil.Chunk.UnitDefn (unitWrapper, MayHaveUnit(getUnit))
import Language.Drasil.Expr (Expr(..))
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Space (Space)
import Language.Drasil.Stages (Stage)
import Language.Drasil.Symbol (Symbol)

-- | ConstrainedChunks are 'Symbolic Quantities'
-- with 'Constraints' and maybe a typical value
data ConstrainedChunk = ConstrainedChunk { _qd :: QuantityDict
                                         , _constr :: [Constraint]
                                         , _reasV :: Maybe Expr
                                         }
makeLenses ''ConstrainedChunk

instance HasUID        ConstrainedChunk where uid = qd . uid
instance NamedIdea     ConstrainedChunk where term = qd . term
instance Idea          ConstrainedChunk where getA = getA . view qd
instance HasSpace      ConstrainedChunk where typ = qd . typ
instance HasSymbol     ConstrainedChunk where symbol c = symbol (c^.qd)
instance Quantity      ConstrainedChunk where 
instance Constrained   ConstrainedChunk where constraints = constr
instance HasReasVal    ConstrainedChunk where reasVal     = reasV
instance Eq            ConstrainedChunk where c1 == c2 = (c1 ^. qd . uid) == (c2 ^. qd . uid)
instance MayHaveUnit   ConstrainedChunk where getUnit = getUnit . view qd

-- | Creates a constrained unitary chunk from a 'UID', term ('NP'), 'Symbol', unit, 'Space', 'Constraint's, and an 'Expr'
cuc :: (IsUnit u) => String -> NP -> Symbol -> u
  -> Space -> [Constraint] -> Expr -> ConstrainedChunk
cuc i t s u space cs rv = ConstrainedChunk (qw (unitary i t s u space)) cs (Just rv)

-- | Creates a constrained unitary chunk from a 'UID', term ('NP'), 'Symbol', 'Space', 'Constraint's, and a 'Maybe' 'Expr' (no units)
cvc :: String -> NP -> Symbol -> Space -> [Constraint] -> Maybe Expr -> ConstrainedChunk
cvc i des sym space = ConstrainedChunk (qw (vc i des sym space))

-- | Creates a new ConstrainedChunk from either a 'ConstrainedChunk', 'ConstrConcept', 'UncertainChunk', or an 'UncertQ'
cnstrw :: (Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => c -> ConstrainedChunk
cnstrw c = ConstrainedChunk (qw c) (c ^. constraints) (c ^. reasVal)

-- | ConstrConcepts are 'Conceptual Symbolic Quantities'
-- with 'Constraints' and maybe a reasonable value (and no unit!)
data ConstrConcept = ConstrConcept { _defq :: DefinedQuantityDict
                                   , _constr' :: [Constraint]
                                   , _reasV' :: Maybe Expr
                                   }
makeLenses ''ConstrConcept

instance HasUID        ConstrConcept where uid = defq . uid
instance NamedIdea     ConstrConcept where term = defq . term
instance Idea          ConstrConcept where getA = getA . view defq
instance HasSpace      ConstrConcept where typ = defq . typ
instance HasSymbol     ConstrConcept where symbol c = symbol (c^.defq)
instance Quantity      ConstrConcept where 
instance Definition    ConstrConcept where defn = defq . defn
instance ConceptDomain ConstrConcept where cdom = cdom . view defq
instance Constrained   ConstrConcept where constraints  = constr'
instance HasReasVal    ConstrConcept where reasVal      = reasV'
instance Eq            ConstrConcept where c1 == c2 = (c1 ^.defq.uid) == (c2 ^.defq.uid)
instance MayHaveUnit   ConstrConcept where getUnit = getUnit . view defq

-- | Creates a 'ConstrConcept' with a quantitative concept, a list of 'Constraint's and an 'Expr'
constrained' :: (Concept c, MayHaveUnit c, Quantity c) =>
  c -> [Constraint] -> Expr -> ConstrConcept
constrained' q cs rv = ConstrConcept (dqdWr q) cs (Just rv)

-- | Similar to 'constrained'', but defaults 'Maybe' 'Expr' to 'Nothing'
constrainedNRV' :: (Concept c, MayHaveUnit c, Quantity c) =>
  c -> [Constraint] -> ConstrConcept
constrainedNRV' q cs = ConstrConcept (dqdWr q) cs Nothing

--TODO: Document these functions
cuc' :: (IsUnit u) => String -> NP -> String -> Symbol -> u
            -> Space -> [Constraint] -> Expr -> ConstrConcept
cuc' nam trm desc sym un space cs rv =
  ConstrConcept (dqd (cw (ucs nam trm desc sym space un)) sym space uu) cs (Just rv)
  where uu = unitWrapper un

cuc'' :: (IsUnit u) => String -> NP -> String -> (Stage -> Symbol) -> u
            -> Space -> [Constraint] -> Expr -> ConstrConcept
cuc'' nam trm desc sym un space cs rv =
  ConstrConcept (dqd' (dcc nam trm desc) sym space (Just uu)) cs (Just rv)
  where uu = unitWrapper un

cnstrw' :: (Quantity c, Concept c, Constrained c, HasReasVal c, MayHaveUnit c) => c -> ConstrConcept
cnstrw' c = ConstrConcept (dqdWr c) (c ^. constraints) (c ^. reasVal)
