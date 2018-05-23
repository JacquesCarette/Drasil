{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.AssumpChunk 
  ( AssumpChunk(..)
  , assump
  ) where

import Language.Drasil.Classes (HasUID(uid), HasAttributes(attributes),
	HasShortName(shortname))
import Language.Drasil.Chunk.Attribute.Core (Attributes)
import Language.Drasil.Spec (Sentence(..), RefName)
import Language.Drasil.Chunk.Attribute.ShortName

import Control.Lens (makeLenses, (^.))

-- | Assumption chunk type. Has id, what is being assumed, and attributes.
-- Presently assumptions are captured as sentences.
data AssumpChunk = AC 
                 { _aid :: String
                 , assuming :: Sentence
                 , _refName :: ShortNm
                 , _attribs :: Attributes
                 }
makeLenses ''AssumpChunk

instance HasUID        AssumpChunk where uid = aid
instance HasAttributes AssumpChunk where attributes = attribs
instance HasShortName  AssumpChunk where shortname (AC _ _ sn _) = sn
instance Eq            AssumpChunk where a == b = a ^. uid == b ^. uid
  
-- | Smart constructor for Assumption chunks. The second 'Sentence' here is 
-- a short name (attribute).
assump :: String -> Sentence -> RefName -> Attributes -> AssumpChunk
assump i a s att = AC i a s att
