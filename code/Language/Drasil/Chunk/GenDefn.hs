{-# Language TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.GenDefn
  ( GenDefn, gd, gdUnit
  ) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, IsUnit,
  ExprRelat(relat), HasDerivation(derivations), HasReference(getReferences),
  HasLabel(getLabel))
import Language.Drasil.Chunk.References (References)
import Language.Drasil.Chunk.Relation (RelationConcept)
import Language.Drasil.Unit (unitWrapper, UnitDefn)
import Language.Drasil.Chunk.Derivation (Derivation)
import Language.Drasil.Chunk.ShortName (HasShortName(shortname))
import Language.Drasil.Label.Core (Label)

import Control.Lens (makeLenses, view)

-- | A GenDefn is a RelationConcept that may have units
data GenDefn = GD { _relC :: RelationConcept
                  , gdUnit :: Maybe UnitDefn                  
                  , _deri :: Derivation
                  , _ref :: References
                  , _lb :: Label
                  }
makeLenses ''GenDefn

instance HasUID        GenDefn where uid = relC . uid
instance NamedIdea     GenDefn where term = relC . term
instance Idea          GenDefn where getA (GD a _ _ _ _) = getA a
instance Concept       GenDefn where
instance Definition    GenDefn where defn = relC . defn
instance ConceptDomain GenDefn where cdom = relC . cdom
instance ExprRelat     GenDefn where relat = relC . relat
instance HasDerivation GenDefn where derivations = deri
instance HasReference  GenDefn where getReferences = ref
instance HasLabel      GenDefn where getLabel = lb
instance HasShortName  GenDefn where shortname = lb . shortname

gd :: (IsUnit u, ConceptDomain u) => RelationConcept -> Maybe u ->
  Derivation -> Label -> GenDefn
gd r (Just u) derivs lbe = GD r (Just (unitWrapper u)) derivs [] lbe
gd r Nothing derivs lbe = GD r Nothing derivs [] lbe
