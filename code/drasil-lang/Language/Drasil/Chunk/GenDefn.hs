{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.GenDefn ( GenDefn, gd', gd'') where

import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname),
  HasRefAddress(getRefAdd))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), IsUnit,
  ExprRelat(relat), HasDerivation(derivations),
  HasAdditionalNotes(getNotes), CommonIdea(abrv))
import Language.Drasil.Classes.Document (HasCitation(getCitations))
import Data.Drasil.IdeaDicts (gendef)
import Language.Drasil.Chunk.Citation (Citation) 
import Language.Drasil.Chunk.CommonIdea (prependAbrv)
import Language.Drasil.Chunk.Relation (RelationConcept)
import Language.Drasil.Derivation (Derivation)
import Language.Drasil.Development.Unit (unitWrapper, UnitDefn, MayHaveUnit(getUnit))
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.ShortName (ShortName, shortname')

import Control.Lens (makeLenses, view)

-- | A GenDefn is a RelationConcept that may have units
data GenDefn = GD { _relC  :: RelationConcept
                  , gdUnit :: Maybe UnitDefn                  
                  , _deri  :: Derivation
                  , _cit   :: [Citation]
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
instance HasCitation        GenDefn where getCitations = cit
instance HasShortName       GenDefn where shortname = view sn
instance HasRefAddress      GenDefn where getRefAdd = view ra
instance HasAdditionalNotes GenDefn where getNotes = notes
instance MayHaveUnit        GenDefn where getUnit = gdUnit
instance CommonIdea         GenDefn where abrv _ = abrv gendef

gd' :: (IsUnit u, ConceptDomain u) => RelationConcept -> Maybe u ->
  Derivation -> [Citation] -> String -> [Sentence] -> GenDefn
gd' r u derivs ref sn_ note = 
  GD r (fmap unitWrapper u) derivs ref (shortname' sn_) (prependAbrv gendef sn_) note

gd'' :: RelationConcept -> [Citation] -> String -> [Sentence] -> GenDefn
gd'' r ref sn_ note = GD r Nothing  [] ref (shortname' sn_) (prependAbrv gendef sn_) note
