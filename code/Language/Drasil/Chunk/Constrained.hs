{-# Language GADTs, TemplateHaskell #-}
module Language.Drasil.Chunk.Constrained (
    Constrained(..), HasReasVal(..)
  , Constraint(..), ConstraintReason(..)
  , ConstrainedChunk(..)
  , ConstrConcept(..)
  , physc, sfwrc, enumc, isPhysC, isSfwrC, renderC
  , constrained, cuc, cvc, constrained', cuc', constrainedNRV'
  , cnstrw
  , Reason(..), TheoryConstraint(..)
  ) where

import Control.Lens (Lens', (^.), makeLenses, view)
import Language.Drasil.Expr (Expr(..), RealInterval(..), Relation, Inclusive(..),
  ($<), ($<=), ($>), ($>=))
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.DefinedQuantity
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Unitary
import Language.Drasil.Chunk.VarChunk
import Language.Drasil.Chunk.Unital (ucs)
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.SymbolForm
import Language.Drasil.Unit
import Language.Drasil.NounPhrase
import Language.Drasil.Space
import Language.Drasil.Symbol
import Language.Drasil.Chunk

-- | A Constrained is a 'Quantity' that has value constraints
class Quantity c => Constrained c where
  constraints :: Lens' c [Constraint]

-- | A HasReasVal is a 'Quantity' that could have a reasonable value
class Quantity c => HasReasVal c where
  reasVal     :: Lens' c (Maybe Expr)

-- AssumedCon are constraints that come from assumptions as opposed to theory invariants.
-- This might be an artificial distinction as they may be "the same"
data Reason = Invariant | AssumedCon
data TheoryConstraint = TCon Reason Relation 

data ConstraintReason = Physical | Software
data Constraint where
  Range          :: ConstraintReason -> RealInterval -> Constraint
  EnumeratedReal :: ConstraintReason -> [Double]     -> Constraint
  EnumeratedStr  :: ConstraintReason -> [String]     -> Constraint

-- by default, physical and software constraints are ranges
physc :: RealInterval -> Constraint
physc = Range Physical

sfwrc :: RealInterval -> Constraint
sfwrc = Range Software

-- but also for enumeration of values; right now, always physical
enumc :: [Double] -> Constraint
enumc = EnumeratedReal Physical

-- helpful for filtering for Physical / Software constraints
isPhysC, isSfwrC :: Constraint -> Bool
isPhysC (Range Physical _) = True
isPhysC (EnumeratedReal Physical _) = True
isPhysC (EnumeratedStr Physical _) = True
isPhysC _ = False

isSfwrC (Range Software _) = True
isSfwrC (EnumeratedReal Software _) = True
isSfwrC (EnumeratedStr Software _) = True
isSfwrC _ = False

renderC :: Chunk c => c -> Constraint -> Expr
renderC s (Range _ rr)          = renderRealInt s rr
renderC s (EnumeratedReal _ rr) = IsIn (C s) (DiscreteD rr)
renderC s (EnumeratedStr _ rr)  = IsIn (C s) (DiscreteS rr)

-- FIXME: bit of a hack for display purposes here
renderRealInt :: Chunk c => c -> RealInterval -> Expr
renderRealInt s (Bounded (Inc a) (Inc b)) = a $<= C s $<= b
renderRealInt s (Bounded (Inc a) (Exc b)) = a $<= C s $<  b
renderRealInt s (Bounded (Exc a) (Inc b)) = a $<  C s $<= b
renderRealInt s (Bounded (Exc a) (Exc b)) = a $<  C s $<  b
renderRealInt s (UpTo (Inc a))    = C s $<= a
renderRealInt s (UpTo (Exc a))    = C s $< a
renderRealInt s (UpFrom (Inc a))  = C s $>= a
renderRealInt s (UpFrom (Exc a))  = C s $>  a

-- | ConstrainedChunks are 'Symbolic Quantities'
-- with 'Constraints' and maybe typical value
data ConstrainedChunk = ConstrainedChunk {
  _qd :: QuantityDict, _constr :: [Constraint], _reasV :: Maybe Expr}
makeLenses ''ConstrainedChunk

instance Chunk       ConstrainedChunk where uid = qd . uid
instance NamedIdea   ConstrainedChunk where term = qd . term
instance Idea        ConstrainedChunk where getA = getA . view qd
instance HasSpace    ConstrainedChunk where typ = qd . typ
instance HasSymbol   ConstrainedChunk where symbol s (ConstrainedChunk c _ _) = symbol s c
instance Quantity    ConstrainedChunk where getUnit = getUnit . view qd
instance Constrained ConstrainedChunk where constraints = constr
instance HasReasVal  ConstrainedChunk where reasVal     = reasV
instance Eq          ConstrainedChunk where c1 == c2 = (c1 ^. qd . uid) == (c2 ^. qd . uid)

-- | Creates a constrained chunk from a symbolic quantity
constrained :: (Quantity c) => c -> [Constraint] -> Expr -> ConstrainedChunk
constrained q cs ex = ConstrainedChunk (qw q) cs (Just ex)

-- | Creates a constrained unitary
cuc :: IsUnit u => String -> NP -> Symbol -> u
                -> Space -> [Constraint] -> Expr -> ConstrainedChunk
cuc i t s u space cs rv =
  ConstrainedChunk (qw $ unitary i t s (unitWrapper u) space) cs (Just rv)

-- | Creates a constrained varchunk
cvc :: String -> NP -> Symbol -> Space -> [Constraint] -> Expr -> ConstrainedChunk
cvc i des sym space cs rv = ConstrainedChunk (qw $ vc i des sym space) cs (Just rv)


-- | ConstrConcepts are 'Conceptual Symbolic Quantities'
-- with 'Constraints' and maybe a reasonable value
data ConstrConcept = ConstrConcept { _defq :: DefinedQuantityDict,
  _constr' :: [Constraint], _reasV' :: Maybe Expr}
makeLenses ''ConstrConcept

instance Chunk         ConstrConcept where uid = defq . uid
instance NamedIdea     ConstrConcept where term = defq . term
instance Idea          ConstrConcept where getA = getA . view defq
instance HasSpace      ConstrConcept where typ = defq . typ
instance HasSymbol     ConstrConcept where symbol s (ConstrConcept c _ _) = symbol s c
instance Quantity      ConstrConcept where getUnit = getUnit . view defq
instance Definition    ConstrConcept where defn = defq . defn
instance ConceptDomain ConstrConcept where cdom = defq . cdom
instance Concept       ConstrConcept where
instance Constrained   ConstrConcept where constraints  = constr'
instance HasReasVal    ConstrConcept where reasVal      = reasV'
instance Eq            ConstrConcept where c1 == c2 = (c1 ^.defq.uid) == (c2 ^.defq.uid)

constrained' :: (Quantity c, Concept c) => c -> [Constraint] -> Expr -> ConstrConcept
constrained' q cs rv = ConstrConcept (cqs q) cs (Just rv)

constrainedNRV' :: (Quantity c, Concept c) => c -> [Constraint] -> ConstrConcept
constrainedNRV' q cs = ConstrConcept (cqs q) cs Nothing

cuc' :: (IsUnit u) => String -> NP -> String -> Symbol -> u
                  -> Space -> [Constraint] -> Expr -> ConstrConcept
cuc' nam trm desc sym un space cs rv =
  ConstrConcept (cqs $ ucs nam trm desc sym un space) cs (Just rv)

cnstrw :: (Constrained c, HasReasVal c) => c -> ConstrainedChunk
cnstrw c = ConstrainedChunk (qw c) (c ^. constraints) (c ^. reasVal)
