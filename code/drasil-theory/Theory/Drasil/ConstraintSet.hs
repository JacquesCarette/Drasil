{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, PostfixOperators  #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Defines types used in models and theories.
module Theory.Drasil.ConstraintSet (
  -- * Type
  ConstraintSet,
  -- * Constructor
  mkConstraintSet) where

import Control.Lens ((^.), makeLenses)

import Language.Drasil
import qualified Data.List.NonEmpty as NE

-- | 'ConstraintSet's are sets of invariants that always hold for underlying domains.
data ConstraintSet e = CL {
    _con  :: ConceptChunk,
    _invs :: Express e => NE.NonEmpty e
}
makeLenses ''ConstraintSet

-- | Finds the 'UID' of the 'ConstraintSet'.
instance HasUID        (ConstraintSet e) where uid   = con . uid
-- | Finds the term ('NP') of the 'ConstraintSet'.
instance NamedIdea     (ConstraintSet e) where term  = con . term
-- | Finds the idea of the 'ConstraintSet'.
instance Idea          (ConstraintSet e) where getA  = getA . (^. con)
-- | Finds the definition of the 'ConstraintSet'.
instance Definition    (ConstraintSet e) where defn  = con . defn
-- | Finds the domain of the 'ConstraintSet'.
instance ConceptDomain (ConstraintSet e) where cdom  = cdom . (^. con)
-- | The complete 'ModelExpr' of a ConstraintSet is the logical conjunction of
--   all the underlying relations (e.g., `a $&& b $&& ... $&& z`).
instance Express e => Express (ConstraintSet e) where
    express = andMEs . map express . NE.toList . (^. invs)

-- | Smart constructor for building ConstraintSets
mkConstraintSet :: Express e => ConceptChunk -> NE.NonEmpty e -> ConstraintSet e
mkConstraintSet = CL
