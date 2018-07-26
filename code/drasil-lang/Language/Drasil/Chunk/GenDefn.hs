{-# Language TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.GenDefn
  ( GenDefn, gd, gdUnit, gd', gd''
  ) where
import Control.Lens (makeLenses, view)

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, IsUnit,
  ExprRelat(relat), HasDerivation(derivations), HasReference(getReferences),
  HasAdditionalNotes(getNotes))
import Language.Drasil.Chunk.Derivation (Derivation)
import Language.Drasil.Chunk.References (References)
import Language.Drasil.Chunk.Relation (RelationConcept)
import Language.Drasil.Chunk.ShortName (ShortName, HasShortName(shortname), shortname')
import Language.Drasil.Development.Unit (unitWrapper, UnitDefn, MayHaveUnit(getUnit))
import Language.Drasil.Spec (Sentence)



-- | A GenDefn is a RelationConcept that may have units
data GenDefn = GD { _relC :: RelationConcept
                  , gdUnit :: Maybe UnitDefn                  
                  , _deri :: Derivation
                  , _ref :: References
                  , _refName :: ShortName
                  , _notes :: Maybe [Sentence]
                  }
makeLenses ''GenDefn

instance HasUID        GenDefn where uid = relC . uid
instance NamedIdea     GenDefn where term = relC . term
instance Idea          GenDefn where getA (GD a _ _ _ _ _) = getA a
instance Concept       GenDefn where
instance Definition    GenDefn where defn = relC . defn
instance ConceptDomain GenDefn where cdom = relC . cdom
instance ExprRelat     GenDefn where relat = relC . relat
instance HasDerivation GenDefn where derivations = deri
instance HasReference  GenDefn where getReferences = ref
instance HasShortName  GenDefn where shortname = view refName
instance HasAdditionalNotes GenDefn where getNotes = notes
instance MayHaveUnit   GenDefn where getUnit u = gdUnit u

gd :: (IsUnit u, ConceptDomain u) => RelationConcept -> Maybe u ->
  Derivation -> String -> GenDefn
gd r (Just u) derivs sn = GD r (Just (unitWrapper u)) derivs [] (shortname' sn) Nothing
gd r Nothing derivs sn = GD r Nothing derivs [] (shortname' sn) Nothing

gd' :: (IsUnit u, ConceptDomain u) => RelationConcept -> Maybe u ->
  Derivation -> String -> [Sentence] -> GenDefn
gd' r (Just u) derivs sn note = GD r (Just (unitWrapper u)) derivs [] (shortname' sn) (Just note)
gd' r Nothing derivs sn note = GD r Nothing derivs [] (shortname' sn) (Just note)

gd'' :: RelationConcept -> String -> [Sentence] -> GenDefn
gd'' r sn []   = GD r (Nothing :: Maybe UnitDefn) ([] :: Derivation) [] (shortname' sn) Nothing
gd'' r sn note = GD r (Nothing :: Maybe UnitDefn) ([] :: Derivation) [] (shortname' sn) (Just note)