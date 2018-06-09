{-# Language TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.Constrained (
    ConstrainedChunk(..)
  , ConstrConcept(..)
  , constrained, cuc, cvc, cvc', constrained', cuc', constrainedNRV'
  , cnstrw
  ) where

import Control.Lens ((^.), makeLenses, view)

import Language.Drasil.Chunk.Constrained.Core (Constraint(..))
import Language.Drasil.Expr (Expr(..))
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.DefinedQuantity
import Language.Drasil.Chunk.Unitary
import Language.Drasil.Chunk.VarChunk
import Language.Drasil.Chunk.Unital (ucs)
import Language.Drasil.Chunk.Concept
import Language.Drasil.NounPhrase
import Language.Drasil.Unit (unitWrapper)
import Language.Drasil.Space
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, HasSymbol(symbol),
  IsUnit, Constrained(constraints), HasReasVal(reasVal))

-- | ConstrainedChunks are 'Symbolic Quantities'
-- with 'Constraints' and maybe typical value
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
instance Quantity      ConstrainedChunk where getUnit = getUnit . view qd
instance Constrained   ConstrainedChunk where constraints = constr
instance HasReasVal    ConstrainedChunk where reasVal     = reasV
instance Eq            ConstrainedChunk where c1 == c2 = (c1 ^. qd . uid) == (c2 ^. qd . uid)

-- | Creates a constrained chunk from a symbolic quantity
constrained :: (Quantity c) => c -> [Constraint] -> Expr -> ConstrainedChunk
constrained q cs ex = ConstrainedChunk (qw q) cs (Just ex)

-- | Creates a constrained unitary
cuc :: (IsUnit u, ConceptDomain u) => String -> NP -> Symbol -> u
                -> Space -> [Constraint] -> Expr  -> ConstrainedChunk
cuc i t s u space cs rv =
  ConstrainedChunk (qw (unitary i t s u space)) cs (Just rv)

-- | Creates a constrained varchunk
cvc :: String -> NP -> Symbol -> Space -> [Constraint] -> Expr -> ConstrainedChunk
cvc i des sym space cs rv = ConstrainedChunk (qw (vc i des sym space)) cs (Just rv)

cvc' :: String -> NP -> Symbol -> Space -> [Constraint] -> ConstrainedChunk
cvc' i des sym space cs = ConstrainedChunk (qw (vc i des sym space)) cs Nothing

cnstrw :: (Quantity c, Constrained c, HasReasVal c) => c -> ConstrainedChunk
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
instance Quantity      ConstrConcept where getUnit = getUnit . view defq
instance Definition    ConstrConcept where defn = defq . defn
instance ConceptDomain ConstrConcept where
  cdom = defq . cdom
instance Concept       ConstrConcept where
instance Constrained   ConstrConcept where constraints  = constr'
instance HasReasVal    ConstrConcept where reasVal      = reasV'
instance Eq            ConstrConcept where c1 == c2 = (c1 ^.defq.uid) == (c2 ^.defq.uid)

constrained' :: (HasSpace c, HasSymbol c, Concept c, Quantity c) =>
  c -> [Constraint] -> Expr -> ConstrConcept
constrained' q cs rv = ConstrConcept (dqd' (cw q) (symbol q) (q ^. typ) (getUnit q)) cs (Just rv)

constrainedNRV' :: (HasSpace c, HasSymbol c, Concept c, Quantity c) =>
  c -> [Constraint] -> ConstrConcept
constrainedNRV' q cs = ConstrConcept (dqd' (cw q) (symbol q) (q ^. typ) (getUnit q)) cs Nothing

cuc' :: (IsUnit u, ConceptDomain u) => String -> NP -> String -> Symbol -> u
            -> Space -> [Constraint] -> Expr -> ConstrConcept
cuc' nam trm desc sym un space cs rv =
  ConstrConcept (dqd (cw (ucs nam trm desc sym un space)) sym space (Just uu)) cs (Just rv)
  where uu = unitWrapper un
