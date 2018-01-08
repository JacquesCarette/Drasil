{-# Language GADTs #-}
module Language.Drasil.Chunk.AssumpChunk 
  ( assuming
  , AssumpChunk
  , ac, ac'
  ) where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Spec (Sentence)

import Control.Lens ((^.))

import Prelude hiding (id)

-- | Assumption chunk type. Has id, what is being assumed, and attributes.
-- Presently assumptions are captured as sentences.
data AssumpChunk = AC 
                 { _id :: String
                 , assuming :: Sentence
                 , _as :: Attributes
                 }

instance Chunk AssumpChunk where
  id f (AC a b c) = fmap (\x -> AC x b c) (f a)
instance HasAttributes AssumpChunk where
  attributes f (AC a b c) = fmap (\x -> AC a b x) (f c)
instance Eq AssumpChunk where
  a == b = a ^. id == b ^. id
  
-- | Smart constructor for Assumption chunks. The second 'Sentence' here is 
-- a short name (attribute).
ac :: String -> Sentence -> Sentence -> AssumpChunk
ac i a s = AC i a [ShortName s]

-- | Smart constructor for Assumptions lacking a short name.
ac' :: String -> Sentence -> AssumpChunk
ac' i a = AC i a []
