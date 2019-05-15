{-# Language TemplateHaskell, TypeFamilies #-}
module Theory.Drasil.GenDefn (GenDefn, gd, gdUnit, gd', gd'', gdNoUnitDef) where

import Language.Drasil
import Control.Lens (makeLenses)


-- | A GenDefn is a RelationConcept that may have units
data GenDefn = GD { _relC :: RelationConcept
                  , gdUnit :: Maybe UnitDefn                  
                  , _deri :: Derivation
                  , _ref :: [Reference]
                  , _lb :: Label
                  , _notes :: Maybe [Sentence]
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
instance HasReference       GenDefn where getReferences = ref
instance HasLabel           GenDefn where getLabel = lb
instance HasShortName       GenDefn where shortname = lb . shortname
instance HasAdditionalNotes GenDefn where getNotes = notes
instance MayHaveUnit        GenDefn where getUnit u = gdUnit u

gd :: (IsUnit u, ConceptDomain u) => RelationConcept -> Maybe u ->
  Derivation -> [Reference] -> Label -> GenDefn
gd r (Just u) derivs refs lbe = GD r (Just (unitWrapper u)) derivs refs lbe Nothing
gd r Nothing  derivs refs lbe = GD r Nothing                derivs refs lbe Nothing

gdNoUnitDef :: RelationConcept -> Derivation -> [Reference] -> Label -> GenDefn
gdNoUnitDef r derivs refs lbe = GD r Nothing derivs refs lbe Nothing

gd' :: (IsUnit u, ConceptDomain u) => RelationConcept -> Maybe u ->
  Derivation -> [Reference] -> String -> [Sentence] -> GenDefn
gd' r (Just u) derivs refs sn note = GD r (Just (unitWrapper u)) derivs refs 
  (mkLabelSame sn (Def General)) (Just note)
gd' r Nothing  derivs refs sn note = GD r Nothing                derivs refs 
  (mkLabelSame sn (Def General)) (Just note)

gd'' :: RelationConcept -> [Reference] -> String -> [Sentence] -> GenDefn
gd'' r refs sn []   = GD r (Nothing :: Maybe UnitDefn) ([] :: Derivation) refs 
  (mkLabelSame sn (Def General)) Nothing
gd'' r refs sn note = GD r (Nothing :: Maybe UnitDefn) ([] :: Derivation) refs 
  (mkLabelSame sn (Def General)) (Just note)
