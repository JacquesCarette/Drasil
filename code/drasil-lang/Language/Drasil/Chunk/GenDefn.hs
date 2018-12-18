{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.GenDefn ( GenDefn, gd', gd'') where

import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname),
  HasRefAddress(getRefAdd))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA), HasLabel(getLabel),
  Definition(defn), ConceptDomain(cdom), Concept, IsUnit,
  ExprRelat(relat), HasDerivation(derivations),
  HasAdditionalNotes(getNotes), CommonIdea(abrv))
import Data.Drasil.IdeaDicts (softEng)
import Language.Drasil.Chunk.Citation (Citation, HasCitation(getCitations))
import Language.Drasil.Chunk.Relation (RelationConcept)
import Language.Drasil.Derivation (Derivation)
import Language.Drasil.Development.Unit (unitWrapper, UnitDefn, MayHaveUnit(getUnit))
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.RefProg(Reference, makeGDRef)
import Control.Lens (makeLenses, view)
import Language.Drasil.Chunk.CommonIdea (CI, commonIdeaWithDict)
import Language.Drasil.NounPhrase (cn')

-- | A GenDefn is a RelationConcept that may have units
data GenDefn = GD { _relC  :: RelationConcept
                  , gdUnit :: Maybe UnitDefn                  
                  , _deri  :: Derivation
                  , _cit   :: [Citation]
                  , _re    :: Reference
                  , _notes :: [Sentence]
                  , _ci    :: CI
                  }
makeLenses ''GenDefn

instance HasUID             GenDefn where uid = relC . uid
instance NamedIdea          GenDefn where term = relC . term
instance Idea               GenDefn where getA = getA . view relC
instance Concept            GenDefn where
instance Definition         GenDefn where defn = relC . defn
instance ConceptDomain      GenDefn where cdom = relC . cdom
instance ExprRelat          GenDefn where relat = relC . relat
instance HasDerivation      GenDefn where derivations = deri
instance HasCitation        GenDefn where getCitations = cit
instance HasLabel           GenDefn where getLabel = re
instance HasShortName       GenDefn where shortname = shortname . view re
instance HasRefAddress      GenDefn where getRefAdd = getRefAdd . view re
instance HasAdditionalNotes GenDefn where getNotes = notes
instance MayHaveUnit        GenDefn where getUnit = gdUnit
instance CommonIdea         GenDefn where abrv = abrv . view ci

gendef :: CI
gendef    = commonIdeaWithDict "gendef"    (cn' "General Definition")                    "GD"        [softEng]

gd' :: (IsUnit u, ConceptDomain u) => RelationConcept -> Maybe u ->
  Derivation -> [Citation] -> String -> [Sentence] -> GenDefn
gd' r u derivs ref sn note = 
  GD r (fmap unitWrapper u) derivs ref (makeGDRef sn) note gendef

gd'' :: RelationConcept -> [Citation] -> String -> [Sentence] -> GenDefn
gd'' r ref sn note = GD r Nothing  [] ref (makeGDRef sn) note gendef
