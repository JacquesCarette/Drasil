{-# Language TemplateHaskell #-}
module Theory.Drasil.GenDefn (GenDefn, gd, gdNoRefs) where

import Language.Drasil
import Data.Drasil.IdeaDicts (gendef)

import Control.Lens (makeLenses, view)

-- | A GenDefn is a RelationConcept that may have units
data GenDefn = GD { _relC  :: RelationConcept
                  , gdUnit :: Maybe UnitDefn                  
                  , _deri  :: Derivation
                  , _ref   :: [Reference]
                  , _sn    :: ShortName
                  , _ra    :: String -- RefAddr
                  , _notes :: [Sentence]
                  }
makeLenses ''GenDefn

instance HasUID             GenDefn where uid = relC . uid
instance NamedIdea          GenDefn where term = relC . term
instance Idea               GenDefn where getA = getA . view relC
instance Definition         GenDefn where defn = relC . defn
instance ConceptDomain      GenDefn where cdom = cdom . view relC
instance ExprRelat          GenDefn where relat = relC . relat
instance HasDerivation      GenDefn where derivations = deri
instance HasReference       GenDefn where getReferences = ref
instance HasShortName       GenDefn where shortname = view sn
instance HasRefAddress      GenDefn where getRefAdd = view ra
instance HasAdditionalNotes GenDefn where getNotes = notes
instance MayHaveUnit        GenDefn where getUnit = gdUnit
instance CommonIdea         GenDefn where abrv _ = abrv gendef
instance Referable          GenDefn where
  refAdd      = getRefAdd
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

gd :: (IsUnit u) => RelationConcept -> Maybe u ->
  Derivation -> [Reference] -> String -> [Sentence] -> GenDefn
gd r u derivs refs sn_ = 
  GD r (fmap unitWrapper u) derivs refs (shortname' sn_) (prependAbrv gendef sn_)

gdNoRefs :: (IsUnit u) => RelationConcept -> Maybe u ->
  Derivation -> String -> [Sentence] -> GenDefn
gdNoRefs r u derivs sn_ = 
  GD r (fmap unitWrapper u) derivs [] (shortname' sn_) (prependAbrv gendef sn_)
