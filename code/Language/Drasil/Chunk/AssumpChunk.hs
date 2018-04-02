{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.AssumpChunk 
  ( AssumpChunk(..)
  , ac, ac'
  ) where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Spec (Sentence(..), RefName)

import Control.Lens (makeLenses, (^.))

-- | Assumption chunk type. Has id, what is being assumed, and attributes.
-- Presently assumptions are captured as sentences.
data AssumpChunk = AC 
                 { _aid :: String
                 , assuming :: Sentence
                 , _refName :: RefName -- HACK for refs?. No spaces/special chars allowed
                 , _attribs :: Attributes
                 }
makeLenses ''AssumpChunk

instance Chunk AssumpChunk where uid = aid
instance HasAttributes AssumpChunk where attributes = attribs
instance Eq AssumpChunk where
  a == b = a ^. uid == b ^. uid
  
-- | Smart constructor for Assumption chunks. The second 'Sentence' here is 
-- a short name (attribute).
ac :: String -> Sentence -> RefName -> AssumpChunk
ac i a s = AC i a s [ShortName s]

-- | Smart constructor for Assumptions lacking a short name. FIXME: Remove this
-- and make shortnames mandatory.
ac' :: String -> Sentence -> AssumpChunk
ac' i a = AC i a (S i) [ShortName (S i)] -- FIXME: HACK
