{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.AssumpChunk ( AssumpChunk(..) , assump) where

import Data.Drasil.IdeaDicts (softEng)
import Language.Drasil.Chunk.CommonIdea (CI, commonIdeaWithDict, prependAbrv)
import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname),
  HasRefAddress(getRefAdd))
import Language.Drasil.Classes (ConceptDomain(cdom), CommonIdea(abrv), NamedIdea(term))
import Language.Drasil.NounPhrase (cn')
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.ShortName (ShortName, shortname')
import Language.Drasil.UID (UID)

import Control.Lens (makeLenses, (^.), view)

-- | Assumption chunk type. Has id, what is being assumed, and a shortname.
-- Presently assumptions are captured as sentences.
data AssumpChunk = AC 
                 { _uu :: UID
                 , assuming :: Sentence
                 , lbl :: ShortName
                 , ra  :: String
                 , _ci :: CI -- keep for now, will have to refactor this too
                 }
makeLenses ''AssumpChunk

instance Eq            AssumpChunk where a == b = a ^. uid == b ^. uid
instance HasUID        AssumpChunk where uid = uu
instance HasRefAddress AssumpChunk where getRefAdd = ra
instance HasShortName  AssumpChunk where shortname = lbl
instance ConceptDomain AssumpChunk where cdom _ = cdom assumption
instance NamedIdea     AssumpChunk where term = ci . term
instance CommonIdea    AssumpChunk where abrv = abrv . view ci

assumption :: CI
assumption  = commonIdeaWithDict "assumption"  (cn' "assumption")                                  "A"         [softEng]
-- | Smart constructor for Assumption chunks.
assump :: UID -> Sentence -> String -> AssumpChunk
assump u s r = AC u s (shortname' r) (prependAbrv assumption r) assumption
