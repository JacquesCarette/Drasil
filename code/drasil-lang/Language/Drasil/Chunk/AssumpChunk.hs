{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.AssumpChunk 
  ( AssumpChunk(..)
  , assump
  ) where

import Language.Drasil.UID (UID)
import Language.Drasil.Classes (HasUID(uid), HasRefAddress(getRefAdd))
import Language.Drasil.Spec (Sentence)
import Language.Drasil.Chunk.ShortName (HasShortName(shortname))
import Control.Lens (makeLenses, (^.))
import Language.Drasil.Label.Core (Label, getAdd)
import Language.Drasil.Classes (HasLabel(getLabel))
import Language.Drasil.RefTypes (RefType(Assump))
import Language.Drasil.Label (mkLabelRAAssump')

-- | Assumption chunk type. Has id, what is being assumed, and a shortname.
-- Presently assumptions are captured as sentences.
data AssumpChunk = AC 
                 { _aid :: UID
                 , assuming :: Sentence
                 , _lbl :: Label
                 }
makeLenses ''AssumpChunk

instance HasUID        AssumpChunk where uid = aid
instance Eq            AssumpChunk where a == b = a ^. uid == b ^. uid
instance HasLabel      AssumpChunk where getLabel = lbl
instance HasShortName  AssumpChunk where shortname = lbl . shortname

-- | Smart constructor for Assumption chunks.
-- FIXME: is it safe to assume the correct label constructor will be
--        used to build the passed in label?
assump :: String -> Sentence -> Label -> AssumpChunk
assump i a lb = AC i a lb
