{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.GenDefn ( GenDefn, gd', gd'') where

import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname),
  HasRefAddress(getRefAdd))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), IsUnit,
  ExprRelat(relat), HasDerivation(derivations), Referable(refAdd, renderRef),
  HasAdditionalNotes(getNotes), CommonIdea(abrv), HasReference(getReferences))
import Data.Drasil.IdeaDicts (gendef)
import Language.Drasil.Chunk.CommonIdea (prependAbrv)
import Language.Drasil.Chunk.Relation (RelationConcept)
import Language.Drasil.Label.Type (LblType(RP), prepend)
import Language.Drasil.Derivation (Derivation)
import Language.Drasil.Chunk.UnitDefn (unitWrapper, UnitDefn, MayHaveUnit(getUnit))
import Language.Drasil.RefProg (Reference)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.ShortName (ShortName, shortname')

import Control.Lens (makeLenses, view)

-- | A GenDefn is a RelationConcept that may have units
data GenDefn = GD { _relC  :: RelationConcept
                  , gdUnit :: Maybe UnitDefn                  
                  , _deri  :: Derivation
                  , _ref   :: Maybe [Reference]
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

gd' :: (IsUnit u) => RelationConcept -> Maybe u ->
  Derivation -> Maybe [Reference] -> String -> [Sentence] -> GenDefn
gd' r u derivs refs sn_ = 
  GD r (fmap unitWrapper u) derivs refs (shortname' sn_) (prependAbrv gendef sn_)

gd'' :: RelationConcept -> Maybe [Reference] -> String -> [Sentence] -> GenDefn
gd'' r refs sn_ = GD r Nothing  [] refs (shortname' sn_) (prependAbrv gendef sn_)
