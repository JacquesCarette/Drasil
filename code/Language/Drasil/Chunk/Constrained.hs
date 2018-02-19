{-# Language GADTs, Rank2Types, TemplateHaskell #-}
module Language.Drasil.Chunk.Constrained (
    Constrained(..)
  , Constraint(..), ConstraintReason(..)
  , ConstrainedChunk(..)
  , ConstrConcept(..)
  , physc, sfwrc, enumc, isPhysC, isSfwrC, renderC
  , constrained, cuc, cvc, constrained', cuc', constrainedNRV'
  , cnstrw
  , Reason(..), TheoryConstraint(..)
  ) where

import Control.Lens (Lens', (^.), set, makeLenses)
import Language.Drasil.Expr (Expr(..), RealInterval(..), Relation, Inclusive(..),
  ($<), ($<=), ($>), ($>=))
import Language.Drasil.Chunk.Quantity
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

import Prelude hiding (id)

-- | A Constrained is a 'Quantity' that has value constraints
-- and maybe reasonable value
class Quantity c => Constrained c where
  constraints :: Lens' c [Constraint]
  reasVal     :: Lens' c (Maybe Expr)

data Reason = Invariant | AssumedCon
data TheoryConstraint = 
  TCon Reason Relation -- AssumedCon are constraints that come from assumptions
                       -- as opposed to theory invariants.
                       -- This might be an artificial distinction as they may be "the same"

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

instance Chunk ConstrainedChunk where id = qd . id
instance NamedIdea ConstrainedChunk where term = qd . term
instance Idea ConstrainedChunk where getA (ConstrainedChunk n _ _) = getA n
instance HasSpace ConstrainedChunk where typ = qd . typ
instance HasSymbol ConstrainedChunk where symbol s (ConstrainedChunk c _ _) = symbol s c
instance Quantity ConstrainedChunk where getUnit (ConstrainedChunk c _ _) = getUnit c
instance Constrained ConstrainedChunk where
  constraints = constr
  reasVal     = reasV
instance Eq ConstrainedChunk where
  (ConstrainedChunk c1 _ _) == (ConstrainedChunk c2 _ _) = (c1 ^. id) == (c2 ^. id)

-- | Creates a constrained chunk from a symbolic quantity
constrained :: (Quantity c) => c 
                -> [Constraint] -> Expr -> ConstrainedChunk
constrained q cs ex = ConstrainedChunk (qw q) cs (Just ex)
  
-- | Creates a constrained unitary  
cuc :: Unit u => String -> NP -> Symbol -> u
                -> Space -> [Constraint] -> Expr -> ConstrainedChunk
cuc i t s u space cs rv = 
  ConstrainedChunk (qw $ unitary i t s (unitWrapper u) space) cs (Just rv)

-- | Creates a constrained varchunk
cvc :: String -> NP -> Symbol -> Space 
       -> [Constraint] -> Expr -> ConstrainedChunk
cvc i des sym space cs rv = 
  ConstrainedChunk (qw $ vc i des sym space) cs (Just rv)
  
  
  
-- | ConstrConcepts are 'Conceptual Symbolic Quantities' 
-- with 'Constraints' and maybe a reasonable value
data ConstrConcept where
  ConstrConcept :: (Quantity c, Concept c) => c 
                        -> [Constraint] -> Maybe Expr -> ConstrConcept

instance Chunk ConstrConcept where id = cqslens id
instance NamedIdea ConstrConcept where term = cqslens term
instance Idea ConstrConcept where getA (ConstrConcept n _ _) = getA n
instance HasSpace ConstrConcept where typ = cqslens typ
instance HasSymbol ConstrConcept where symbol s (ConstrConcept c _ _) = symbol s c
instance Quantity ConstrConcept where getUnit (ConstrConcept c _ _) = getUnit c
instance Definition ConstrConcept where defn = cqslens defn
instance ConceptDomain ConstrConcept where cdom = cqslens cdom
instance Concept ConstrConcept where
instance Constrained ConstrConcept where
  constraints f (ConstrConcept a b c) = fmap (\x -> ConstrConcept a x c) (f b)
  reasVal f (ConstrConcept a b c) = fmap (\x -> ConstrConcept a b x) (f c)
instance Eq ConstrConcept where
  (ConstrConcept c1 _ _) == (ConstrConcept c2 _ _) = (c1 ^. id) == (c2 ^. id)

cqslens :: (forall c. (Quantity c, Concept c) => Lens' c a)
           -> Lens' ConstrConcept a
cqslens l f (ConstrConcept a b c) = 
  fmap (\x -> ConstrConcept (set l x a) b c) (f (a ^. l))
  
constrained' :: (Quantity c, Concept c) => c 
                 -> [Constraint] -> Expr -> ConstrConcept
constrained' q cs rv = ConstrConcept q cs (Just rv)

constrainedNRV' :: (Quantity c, Concept c) => c 
                 -> [Constraint] -> ConstrConcept
constrainedNRV' q cs = ConstrConcept q cs Nothing
  
cuc' :: (Unit u) => String -> NP -> String -> Symbol -> u 
                  -> Space -> [Constraint] -> Expr -> ConstrConcept
cuc' nam trm desc sym un space cs rv = 
  ConstrConcept (ucs nam trm desc sym un space) cs (Just rv)

cnstrw :: Constrained c => c -> ConstrainedChunk
cnstrw c = ConstrainedChunk (qw c) (c ^. constraints) (c ^. reasVal)
