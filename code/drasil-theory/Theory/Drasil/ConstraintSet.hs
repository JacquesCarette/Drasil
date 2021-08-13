{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, PostfixOperators  #-}

module Theory.Drasil.ConstraintSet (ConstraintSet, mkConstraintSet) where

import Control.Lens ((^.), makeLenses)

import Language.Drasil
import qualified Data.List.NonEmpty as NE

-- | 'ConstraintSet's are sets of invariants that always hold for underlying domains.
data ConstraintSet = CL {
    _con  :: ConceptChunk,
    _invs :: NE.NonEmpty ModelExpr -- TODO: parameterize?
}
makeLenses ''ConstraintSet

-- | Finds the 'UID' of the 'ConstraintSet'.
instance HasUID        ConstraintSet where uid   = con . uid
-- | Finds the term ('NP') of the 'ConstraintSet'.
instance NamedIdea     ConstraintSet where term  = con . term
-- | Finds the idea of the 'ConstraintSet'.
instance Idea          ConstraintSet where getA  = getA . (^. con)
-- | Finds the definition of the 'ConstraintSet'.
instance Definition    ConstraintSet where defn  = con . defn
-- | Finds the domain of the 'ConstraintSet'.
instance ConceptDomain ConstraintSet where cdom  = cdom . (^. con)
-- | The complete 'ModelExpr' of a ConstraintSet is the logical conjunction of
--   all the underlying relations (e.g., `a $&& b $&& ... $&& z`).
instance Express       ConstraintSet where
    express = andMEs . map express . NE.toList . (^. invs)

-- | Smart constructor for building ConstraintSets
mkConstraintSet :: ConceptChunk -> NE.NonEmpty ModelExpr -> ConstraintSet
mkConstraintSet = CL
