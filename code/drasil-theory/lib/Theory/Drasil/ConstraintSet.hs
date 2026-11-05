{-# LANGUAGE TemplateHaskell #-}
-- | Defines types used in models and theories.
module Theory.Drasil.ConstraintSet (
  -- * Type
   ConstraintSet,
   -- * Constructors
   mkConstraintSet,
) where

import Control.Lens (makeLenses, (^.))
import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Drasil.Database (HasUID(..), HasChunkRefs(..))

-- | 'ConstraintSet's are sets of invariants that always hold for underlying domains.
data ConstraintSet e = CL {
  _con  :: ConceptChunk,
  _invs :: NE.NonEmpty e
}

makeLenses ''ConstraintSet

instance HasChunkRefs e => HasChunkRefs (ConstraintSet e) where
  chunkRefs cs = mconcat
    [ chunkRefs (cs ^. con)
    , chunkRefs (cs ^. invs)
    ]
  {-# INLINABLE chunkRefs #-}

-- | Finds the 'UID' of the 'ConstraintSet'.
instance HasUID        (ConstraintSet e) where uid  = con . uid
-- | Finds the term ('NP') of the 'ConstraintSet'.
instance HasChunkRefs e => NamedIdea (ConstraintSet e) where term = con . term
-- | Finds the idea of the 'ConstraintSet'.
instance HasChunkRefs e => Idea      (ConstraintSet e) where getA = getA . (^. con)
-- | Finds the definition of the 'ConstraintSet'.
instance Definition    (ConstraintSet e) where defn = con . defn
-- | The complete 'ModelExpr' of a ConstraintSet is the logical conjunction of
--   all the underlying relations (e.g., `a $&& b $&& ... $&& z`).
instance Express e => Express (ConstraintSet e) where
  mexpress = mexpress . (^. invs)
-- | Exposes all relations and an expectation of the type of a relation (Bool)
instance RequiresChecking (ConstraintSet Expr) Expr Space where
  requiredChecks cs = (,Boolean) <$> NE.toList (cs ^. invs)

-- | Smart constructor for building ConstraintSets
mkConstraintSet :: ConceptChunk -> NE.NonEmpty e -> ConstraintSet e
mkConstraintSet = CL
