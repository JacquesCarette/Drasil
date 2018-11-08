{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.AssumpChunk ( AssumpChunk(..) , assump) where
import Control.Lens ((^.))

import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname))
import Language.Drasil.Classes (HasLabel(getLabel), ConceptDomain(cdom))
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.UID (UID)
import Language.Drasil.Chunk.CommonIdea (CI, commonIdeaWithDict)
import Language.Drasil.Chunk.NamedIdea (IdeaDict, mkIdea)
import Language.Drasil.NounPhrase (cn')


import Control.Lens (makeLenses, (^.))

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


softEng :: IdeaDict
softEng      = mkIdea  "softEng"        (cn' "Software Engineering")  (Just "SE")

dataDefn :: CI
dataDefn    = commonIdeaWithDict "dataDefn"    (cn' "data definition")                             "DD"        [softEng]
-- | Smart constructor for Assumption chunks.
-- FIXME: is it safe to assume the correct label constructor will be
--        used to build the passed in label?
assump :: String -> Sentence -> Label -> AssumpChunk
assump = (\x y z -> AC x y z dataDefn)
