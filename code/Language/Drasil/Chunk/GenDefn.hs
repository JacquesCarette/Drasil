{-# Language TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.GenDefn
  ( GenDefn, gd, gdUnit
  ) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom,DOM), Concept, IsUnit, 
  HasAttributes(attributes), ExprRelat(relat), HasReference(getReferences),
  HasShortName(shortname), HasDerivation(derivations))
  
import Language.Drasil.Chunk.Attribute.Core (Attributes)
import Language.Drasil.Chunk.Attribute.References (References)
import Language.Drasil.Chunk.Concept (ConceptChunk)
import Language.Drasil.Chunk.Relation (RelationConcept)
import Language.Drasil.Unit (unitWrapper, UnitDefn)
import Language.Drasil.Chunk.Attribute.Derivation

import Control.Lens (makeLenses)

-- | A GenDefn is a RelationConcept that may have units
data GenDefn = GD { _relC :: RelationConcept
                  , gdUnit :: Maybe UnitDefn
		              , _attribs :: Attributes
		              , _deri :: Derivation
                  , _ref :: References
                  }
makeLenses ''GenDefn

instance HasUID        GenDefn where uid = relC . uid
instance NamedIdea     GenDefn where term = relC . term
instance Idea          GenDefn where getA (GD a _ _ _ _) = getA a
instance Concept       GenDefn where
instance Definition    GenDefn where defn = relC . defn
instance ConceptDomain GenDefn where
  type DOM GenDefn = ConceptChunk
  cdom = relC . cdom
instance ExprRelat     GenDefn where relat = relC . relat
instance HasDerivation GenDefn where derivations = deri
instance HasAttributes GenDefn where attributes = attribs
instance HasShortName  GenDefn where
  shortname _ = error "No explicit name given for general definition -- build a custom Ref"
instance HasReference  GenDefn where getReferences = ref

gd :: (IsUnit u, DOM u ~ ConceptChunk) => RelationConcept -> Maybe u -> Derivation -> GenDefn
gd r (Just u) derivs = GD r (Just (unitWrapper u)) [] derivs []
gd r Nothing derivs = GD r Nothing [] derivs []
