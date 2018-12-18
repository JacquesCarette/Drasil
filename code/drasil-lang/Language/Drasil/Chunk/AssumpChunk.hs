{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.AssumpChunk ( AssumpChunk(..) , assump) where

import Data.Drasil.IdeaDicts (softEng)
import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname),
  HasRefAddress(getRefAdd))
import Language.Drasil.Classes (ConceptDomain(cdom), CommonIdea(abrv), NamedIdea(term))
import Language.Drasil.RefProg (Reference)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.Chunk.CommonIdea (CI, commonIdeaWithDict)
import Language.Drasil.NounPhrase (cn')
import Control.Lens (makeLenses, (^.), view)

-- | Assumption chunk type. Has id, what is being assumed, and a shortname.
-- Presently assumptions are captured as sentences.
data AssumpChunk = AC 
                 { assuming :: Sentence
                 , _lbl :: Reference
                 , _ci :: CI
                 }
makeLenses ''AssumpChunk

instance Eq            AssumpChunk where a == b = a ^. uid == b ^. uid
instance HasUID        AssumpChunk where uid = lbl . uid
instance HasRefAddress AssumpChunk where getRefAdd = getRefAdd . view lbl
instance HasShortName  AssumpChunk where shortname = lbl . shortname
instance ConceptDomain AssumpChunk where cdom = ci . cdom
instance NamedIdea     AssumpChunk where term = ci . term
instance CommonIdea    AssumpChunk where abrv = abrv . view ci

assumption :: CI
assumption  = commonIdeaWithDict "assumption"  (cn' "assumption")                                  "A"         [softEng]
-- | Smart constructor for Assumption chunks.
assump :: Reference -> Sentence -> AssumpChunk
assump r s = AC s r assumption
