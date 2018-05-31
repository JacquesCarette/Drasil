{-# Language TemplateHaskell, FlexibleInstances, TypeFamilies #-}
module Language.Drasil.Chunk.Concept.Core where

import Language.Drasil.Spec (Sentence)
import Language.Drasil.Chunk.NamedIdea (IdeaDict)
import Language.Drasil.Chunk.CommonIdea (CI)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(DOM,cdom), Concept, CommonIdea(abrv))

import Control.Lens (makeLenses, (^.), view)

-- === DATA TYPES === --
--- ConceptChunk ---  

data DefnAndDomain a = DAD { _defn' :: Sentence, _cdom' :: [a]}
makeLenses ''DefnAndDomain

-- | The ConceptChunk datatype is a Concept
-- ConDict is not exported, nor are _idea and _dad
data ConceptChunk = ConDict { _idea :: IdeaDict, _dad :: DefnAndDomain ConceptChunk }
makeLenses ''ConceptChunk

instance Definition    (DefnAndDomain a) where defn = defn'
instance ConceptDomain (DefnAndDomain ConceptChunk) where
  type DOM (DefnAndDomain ConceptChunk) = ConceptChunk
  cdom = cdom'

instance Eq            ConceptChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID        ConceptChunk where uid = idea . uid
instance NamedIdea     ConceptChunk where term = idea . term
instance Idea          ConceptChunk where getA = getA . view idea
instance Definition    ConceptChunk where defn = dad . defn'
instance ConceptDomain ConceptChunk where
  type DOM ConceptChunk = ConceptChunk
  cdom = dad . cdom'
instance Concept       ConceptChunk where
 
data CommonConcept = ComConDict { _comm :: CI, _def :: Sentence, _dom :: [ConceptChunk]}
makeLenses ''CommonConcept

instance Eq            CommonConcept where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID        CommonConcept where uid = comm . uid
instance NamedIdea     CommonConcept where term = comm . term
instance Idea          CommonConcept where getA = getA . view comm
instance Definition    CommonConcept where defn = def
instance CommonIdea    CommonConcept where abrv = abrv . view comm
instance ConceptDomain CommonConcept where 
  type DOM CommonConcept = ConceptChunk
  cdom = dom
instance Concept       CommonConcept where

