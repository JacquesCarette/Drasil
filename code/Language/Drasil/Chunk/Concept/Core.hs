{-# Language TemplateHaskell, FlexibleInstances, TypeFamilies #-}
module Language.Drasil.Chunk.Concept.Core where

import Language.Drasil.Spec (Sentence)
import Language.Drasil.Chunk.NamedIdea (IdeaDict)
import Language.Drasil.Chunk.CommonIdea (CI)
import Language.Drasil.UID (UID)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, CommonIdea(abrv))
import Language.Drasil.Chunk.ShortName (HasShortName(shortname), ShortName)

import Control.Lens (makeLenses, (^.), view)

-- === DATA TYPES === --
--- ConceptChunk ---  

data DefnAndDomain = DAD { _defn' :: Sentence, _cdom' :: [UID]}
makeLenses ''DefnAndDomain

-- | The ConceptChunk datatype is a Concept
-- ConDict is not exported, nor are _idea, _dad, and _sn.
data ConceptChunk = ConDict { _idea :: IdeaDict
                            , _dad :: DefnAndDomain
                            , _sn :: Maybe ShortName
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
instance HasShortName  ConceptChunk where
  shortname x = maybe
    (error $ "No ShortName found for ConceptChunk: " ++ (view uid x)) id $
    view sn x
 
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

