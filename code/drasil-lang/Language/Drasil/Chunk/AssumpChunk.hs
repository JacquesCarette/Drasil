{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.AssumpChunk ( AssumpChunk(..) , assump) where

import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname))
import Language.Drasil.Classes (HasLabel(getLabel), ConceptDomain(cdom), CommonIdea(abrv)
  , NamedIdea(term))
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.UID (UID)
import Data.Drasil.IdeaDicts (softEng)
import Language.Drasil.Chunk.CommonIdea (CI, commonIdeaWithDict)
import Language.Drasil.NounPhrase (cn')

import Control.Lens (makeLenses, (^.), view)

-- | Assumption chunk type. Has id, what is being assumed, and a shortname.
-- Presently assumptions are captured as sentences.
data AssumpChunk = AC 
                { _aid :: UID
                 , assuming :: Sentence
                 , _lbl :: Label
                 , _ci :: CI
                 }
makeLenses ''AssumpChunk

instance HasUID        AssumpChunk where uid = aid
instance Eq            AssumpChunk where a == b = a ^. uid == b ^. uid
instance HasLabel      AssumpChunk where getLabel = lbl
instance HasShortName  AssumpChunk where shortname = lbl . shortname
instance ConceptDomain AssumpChunk where cdom = ci . cdom
instance NamedIdea     AssumpChunk where term = ci . term
instance CommonIdea    AssumpChunk where abrv = abrv . view ci

assumption :: CI
assumption  = commonIdeaWithDict "assumption"  (cn' "assumption") "A" [softEng] 

-- | Smart constructor for Assumption chunks.
-- FIXME: is it safe to assume the correct label constructor will be
--        used to build the passed in label?
assump :: String -> Sentence -> Label -> AssumpChunk
assump = (\x y z -> AC x y z assumption)
