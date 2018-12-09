{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.AssumpChunk ( AssumpChunk(..) , assump) where

import Data.Drasil.IdeaDicts (softEng)
import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname),
  HasRefAddress(getRefAdd))
import Language.Drasil.Classes (HasLabel(getLabel), ConceptDomain(cdom), CommonIdea(abrv)
  , NamedIdea(term))
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.Chunk.CommonIdea (CI, commonIdeaWithDict)
import Language.Drasil.NounPhrase (cn')
import Control.Lens (makeLenses, (^.), view)

-- | Assumption chunk type. Has id, what is being assumed, and a shortname.
-- Presently assumptions are captured as sentences.
data AssumpChunk = AC 
                 { assuming :: Sentence
                 , _lbl :: Label
                 , _ci :: CI
                 }
makeLenses ''AssumpChunk

instance HasUID        AssumpChunk where uid = lbl . uid
instance Eq            AssumpChunk where a == b = a ^. uid == b ^. uid
instance HasLabel      AssumpChunk where getLabel = lbl
instance HasRefAddress AssumpChunk where getRefAdd = lbl . getRefAdd
instance HasShortName  AssumpChunk where shortname = lbl . shortname
instance ConceptDomain AssumpChunk where cdom = ci . cdom
instance NamedIdea     AssumpChunk where term = ci . term
instance CommonIdea    AssumpChunk where abrv = abrv . view ci

assumption :: CI
assumption  = commonIdeaWithDict "assumption"  (cn' "assumption")                                  "A"         [softEng]
-- | Smart constructor for Assumption chunks.
assump :: Label -> Sentence -> AssumpChunk
assump l s = AC s l assumption
