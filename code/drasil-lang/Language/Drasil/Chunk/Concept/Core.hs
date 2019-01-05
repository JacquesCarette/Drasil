{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.Concept.Core(ConceptChunk(ConDict), CommonConcept(ComConDict)
  , ConceptInstance(ConInst)
  , sDom)
  where

import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), CommonIdea(abrv), Referable(refAdd, renderRef))
import Language.Drasil.Chunk.CommonIdea (CI)
import Language.Drasil.Chunk.NamedIdea (IdeaDict)
import Language.Drasil.Label.Type (LblType(RP), name, raw, (+::+), defer)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.ShortName (ShortName)
import Language.Drasil.UID (UID)

import Control.Lens (makeLenses, (^.), view)

sDom :: [UID] -> UID
sDom [d] = d
sDom d = error $ "Expected ConceptDomain to have a single domain, found " ++
  show (length d) ++ " instead."

-- | The ConceptChunk datatype is a Concept
data ConceptChunk = ConDict { _idea :: IdeaDict
                            , _defn' :: Sentence
                            , cdom' :: [UID]
                            }
makeLenses ''ConceptChunk

instance Eq            ConceptChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID        ConceptChunk where uid = idea . uid
instance NamedIdea     ConceptChunk where term = idea . term
instance Idea          ConceptChunk where getA = getA . view idea
instance Definition    ConceptChunk where defn = defn'
instance ConceptDomain ConceptChunk where cdom = cdom'

data CommonConcept = ComConDict { _comm :: CI, _def :: Sentence, dom :: [UID]}
makeLenses ''CommonConcept

instance Eq            CommonConcept where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID        CommonConcept where uid = comm . uid
instance NamedIdea     CommonConcept where term = comm . term
instance Idea          CommonConcept where getA = getA . view comm
instance Definition    CommonConcept where defn = def
instance CommonIdea    CommonConcept where abrv = abrv . view comm
instance ConceptDomain CommonConcept where cdom = dom

data ConceptInstance = ConInst { _cc :: ConceptChunk , shnm :: ShortName}
makeLenses ''ConceptInstance

instance Eq            ConceptInstance where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID        ConceptInstance where uid = cc . idea . uid
instance NamedIdea     ConceptInstance where term = cc . idea . term
instance Idea          ConceptInstance where getA = getA . view (cc . idea)
instance Definition    ConceptInstance where defn = cc . defn'
instance ConceptDomain ConceptInstance where cdom = cdom' . view cc
instance HasShortName  ConceptInstance where shortname = shnm
instance Referable     ConceptInstance where
  refAdd l    = l ^. uid
  renderRef l = RP ((defer $ sDom $ cdom l) +::+ raw ": " +::+ name) (l ^. uid)

