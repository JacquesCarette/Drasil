module Language.Drasil.Chunk.ReqChunk 
  ( ReqChunk, ReqType(..)
  , reqType, requires
  , frc, nfrc
  ) where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Spec (Sentence)

import Control.Lens ((^.))
import Prelude hiding (id)

-- We will likely need to differentiate functional/non-functional reqs
-- (or whatever we want to call them) for the future when we parse our 
-- recipes and build the lists/dbs of chunks.

-- FIXME: We need a better way to capture requirement information. Sentences
-- are dead information, and larger structures (like Contents) are display-specific.
-- For now, using sentences to test.

-- | What type of requirement are we dealing with?
data ReqType = FR  -- ^ Functional Requirement
             | NFR -- ^ Non-Functional Requirement
  deriving Eq

-- | Requirement chunk type. Has an id, the type of requirement
-- (Functional/Non-Functional) from 'ReqType', a sentence describing what is
-- required (TODO: Change this), and a list of attributes.
data ReqChunk = RC 
  { _id      :: String
  , reqType  :: ReqType 
  , requires :: Sentence 
  , _atts    :: Attributes
  }
  
instance Chunk ReqChunk where
  id f (RC a b c d) = fmap (\x -> RC x b c d) (f a)
instance HasAttributes ReqChunk where
  attributes f (RC a b c d) = fmap (\x -> RC a b c x) (f d)
instance Eq ReqChunk where
  a == b = a ^. id == b ^. id

-- | Smart constructor for requirement chunks (should not be exported)
rc :: String -> ReqType -> Sentence -> Attributes -> ReqChunk
rc = RC

frc, nfrc :: String -> Sentence -> Attributes -> ReqChunk
-- | Smart constructor for functional requirement chunks.
frc i = rc i FR

-- | Smart constructor for non-functional requirement chunks.
nfrc i = rc i NFR
