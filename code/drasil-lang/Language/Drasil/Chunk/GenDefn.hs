{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.GenDefn ( GenDefn, gd, gdUnit, gd', gd'', gdNoUnitDef) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, IsUnit,
  ExprRelat(relat), HasDerivation(derivations), HasReference(getReferences),
  HasLabel(getLabel), HasAdditionalNotes(getNotes), HasShortName(shortname))

import Language.Drasil.Chunk.Relation (RelationConcept)
import Language.Drasil.Derivation (Derivation)
import Language.Drasil.Development.Unit (unitWrapper, UnitDefn, MayHaveUnit(getUnit))
import Language.Drasil.Spec (Sentence)
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Label (mkLabelSame)
import Language.Drasil.RefTypes(RefType(..), DType(..), Reference)
import Control.Lens (makeLenses)


-- | A GenDefn is a RelationConcept that may have units
data GenDefn = GD { _relC :: RelationConcept
                  , gdUnit :: Maybe UnitDefn                  
                  , _deri :: Derivation
                  , _refs :: [Reference]
                  , _lb :: Label
                  , _notes :: [Sentence]
                  }
makeLenses ''GenDefn

instance HasUID             GenDefn where uid = relC . uid
instance NamedIdea          GenDefn where term = relC . term
instance Idea               GenDefn where getA (GD a _ _ _ _ _) = getA a
instance Concept            GenDefn where
instance Definition         GenDefn where defn = relC . defn
instance ConceptDomain      GenDefn where cdom = relC . cdom
instance ExprRelat          GenDefn where relat = relC . relat
instance HasDerivation      GenDefn where derivations = deri
instance HasReference       GenDefn where getReferences = refs
instance HasLabel           GenDefn where getLabel = lb
instance HasShortName       GenDefn where shortname = lb . shortname
instance HasAdditionalNotes GenDefn where getNotes = notes
instance MayHaveUnit        GenDefn where getUnit u = gdUnit u

gd :: (IsUnit u, ConceptDomain u) => RelationConcept -> Maybe u ->
  Derivation -> [Reference] -> Label -> GenDefn
gd r (Just u) derivs ref lbe = GD r (Just (unitWrapper u)) derivs ref lbe []
gd r Nothing  derivs ref lbe = GD r Nothing                derivs ref lbe []

gdNoUnitDef :: RelationConcept -> Derivation -> [Reference] -> Label -> GenDefn
gdNoUnitDef r derivs ref lbe = GD r Nothing derivs ref lbe []

gd' :: (IsUnit u, ConceptDomain u) => RelationConcept -> Maybe u ->
  Derivation -> [Reference] -> String -> [Sentence] -> GenDefn
gd' r (Just u) derivs ref sn note = GD r (Just (unitWrapper u)) derivs ref (mkLabelSame sn (Def General)) note
gd' r Nothing  derivs ref sn note = GD r Nothing                derivs ref (mkLabelSame sn (Def General)) note

gd'' :: RelationConcept -> [Reference] -> String -> [Sentence] -> GenDefn
gd'' r ref sn note = GD r (Nothing :: Maybe UnitDefn) ([] :: Derivation) ref (mkLabelSame sn (Def General)) note
