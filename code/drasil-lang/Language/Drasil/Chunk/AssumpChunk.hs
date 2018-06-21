{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.AssumpChunk 
  ( AssumpChunk(..)
  , assump
  ) where

import Language.Drasil.UID (UID)
import Language.Drasil.Classes (HasUID(uid))
import Language.Drasil.Spec (Sentence)
import Language.Drasil.Chunk.ShortName (ShortName, HasShortName(shortname), shortname')
import Control.Lens (makeLenses, (^.), view)


-- | Assumption chunk type. Has id, what is being assumed, and a shortname.
-- Presently assumptions are captured as sentences.
data AssumpChunk = AC 
                 { _aid :: UID
                 , assuming :: Sentence
                 , _refName :: ShortName
                 }
makeLenses ''AssumpChunk

instance HasUID        AssumpChunk where uid = aid
instance Eq            AssumpChunk where a == b = a ^. uid == b ^. uid
instance HasShortName  AssumpChunk where shortname = view refName

-- | Smart constructor for Assumption chunks.
assump :: String -> Sentence -> String -> AssumpChunk
assump i a s = AC i a (shortname' s)
