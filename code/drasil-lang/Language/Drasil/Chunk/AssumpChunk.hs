{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.AssumpChunk ( AssumpChunk(..) , assump) where

import Data.Drasil.Concepts.Documentation (assumption)

import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname),
	ConceptDomain(cdom))
import Language.Drasil.Classes (HasLabel(getLabel))
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.UID (UID)

import Control.Lens (makeLenses, (^.))

-- | Assumption chunk type. Has id, what is being assumed, and a shortname.
-- Presently assumptions are captured as sentences.
data AssumpChunk = AC 
                 { _ci :: CI
                 , _aid :: UID
                 , assuming :: Sentence
                 , _lbl :: Label
                 }
makeLenses ''AssumpChunk

instance HasUID        AssumpChunk where uid = aid
instance Eq            AssumpChunk where a == b = a ^. uid == b ^. uid
instance HasLabel      AssumpChunk where getLabel = lbl
instance HasShortName  AssumpChunk where shortname = lbl . shortname
instance ConceptDomain AssumpChunk where cdom = ci ^. cdom

-- | Smart constructor for Assumption chunks.
-- FIXME: is it safe to assume the correct label constructor will be
--        used to build the passed in label?
assump :: String -> Sentence -> Label -> AssumpChunk
assump = AC assumption
