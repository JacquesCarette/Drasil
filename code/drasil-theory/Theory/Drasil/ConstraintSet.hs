{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, PostfixOperators  #-}

module Theory.Drasil.ConstraintSet (ConstraintSet, mkConstraintSet) where

import Control.Lens ((^.), makeLenses)

import Language.Drasil
import qualified Data.List.NonEmpty as NE

-- | 'ConstraintSet's are sets of invariants that always hold for underlying domains.
data ConstraintSet = CL {
    _con  :: ConceptChunk,
    _invs :: NE.NonEmpty Expr
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
-- | The complete Relation of a ConstraintSet is the logical conjunction of all
--   the underlying relations (e.g., `a $&& b $&& ... $&& z`).
instance ExprRelat     ConstraintSet where relat = foldr1 ($&&) . (^. invs)

-- TODO: How shall we display this?
instance Display       ConstraintSet where toDispExpr = multiExpr . NE.toList . (^. invs)

-- | Smart constructor for building ConstraintSets
mkConstraintSet :: ConceptChunk -> NE.NonEmpty Relation -> ConstraintSet
mkConstraintSet = CL
