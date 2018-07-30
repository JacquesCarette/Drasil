{-# Language TemplateHaskell, FlexibleInstances, TypeFamilies #-}
module Language.Drasil.Chunk.Concept.Core where

import Language.Drasil.Spec (Sentence)
import Language.Drasil.Chunk.NamedIdea (IdeaDict)
import Language.Drasil.Chunk.CommonIdea (CI)
import Language.Drasil.UID (UID)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, CommonIdea(abrv))
import Language.Drasil.Chunk.ShortName (HasShortName(shortname), ShortName)
import Language.Drasil.Label.Core (Label)

import Control.Lens (makeLenses, (^.), view)

-- === DATA TYPES === --
--- ConceptChunk ---  

data DefnAndDomain = DAD { _defn' :: Sentence, _cdom' :: [UID]}
makeLenses ''DefnAndDomain

-- | The ConceptChunk datatype is a Concept
-- ConDict is not exported, nor are _idea, and _dad.
data ConceptChunk = ConDict { _idea :: IdeaDict
                            , _dad :: DefnAndDomain
                            }
makeLenses ''ConceptChunk

instance Definition    DefnAndDomain where defn = defn'
instance ConceptDomain DefnAndDomain where cdom = cdom'

instance Eq            ConceptChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID        ConceptChunk where uid = idea . uid
instance NamedIdea     ConceptChunk where term = idea . term
instance Idea          ConceptChunk where getA = getA . view idea
instance Definition    ConceptChunk where defn = dad . defn'
instance ConceptDomain ConceptChunk where cdom = dad . cdom'
instance Concept       ConceptChunk where


data CommonConcept = ComConDict { _comm :: CI, _def :: Sentence, _dom :: [UID]}
makeLenses ''CommonConcept

instance Eq            CommonConcept where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID        CommonConcept where uid = comm . uid
instance NamedIdea     CommonConcept where term = comm . term
instance Idea          CommonConcept where getA = getA . view comm
instance Definition    CommonConcept where defn = def
instance CommonIdea    CommonConcept where abrv = abrv . view comm
instance ConceptDomain CommonConcept where cdom = dom
instance Concept       CommonConcept where

data ConceptInstance = ConInst { _cc :: ConceptChunk
                               , _shnm :: ShortName}
makeLenses ''ConceptInstance

instance Eq            ConceptInstance where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID        ConceptInstance where uid = cc . idea . uid
instance NamedIdea     ConceptInstance where term = cc . idea . term
instance Idea          ConceptInstance where getA = getA . view (cc . idea)
instance Definition    ConceptInstance where defn = cc . dad . defn'
instance ConceptDomain ConceptInstance where cdom = cc . dad . cdom'
instance Concept       ConceptInstance where
instance HasShortName  ConceptInstance where shortname = shnm
