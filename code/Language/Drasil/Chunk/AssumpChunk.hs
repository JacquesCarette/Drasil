{-# Language GADTs #-}
module Language.Drasil.Chunk.AssumpChunk 
  ( AssumpChunk(..)
  , assuming
  , ac, ac'
  ) where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Spec (Sentence(..), RefName)

import Control.Lens ((^.))

-- | Assumption chunk type. Has id, what is being assumed, and attributes.
-- Presently assumptions are captured as sentences.
data AssumpChunk = AC 
                 { _id :: String
                 , assuming :: Sentence
                 , _refName :: RefName -- HACK for refs?. No spaces/special chars allowed
                 , _as :: Attributes
                 }

instance Chunk AssumpChunk where
  uid f (AC a b c d) = fmap (\x -> AC x b c d) (f a)
instance HasAttributes AssumpChunk where
  attributes f (AC a b c d) = fmap (\x -> AC a b c x) (f d)
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
