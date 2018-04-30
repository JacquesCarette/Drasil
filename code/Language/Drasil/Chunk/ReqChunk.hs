module Language.Drasil.Chunk.ReqChunk 
  ( ReqChunk(..), ReqType(..)
  , frc, nfrc, rc'
  ) where

import Language.Drasil.Classes (HasUID(uid))
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Spec (Sentence, RefName)

import Control.Lens (set, (^.))

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
  
instance Show ReqType where
  show FR  = "FR"
  show NFR = "NFR"

-- | Requirement chunk type. Has an id, the type of requirement
-- (Functional/Non-Functional) from 'ReqType', a sentence describing what is
-- required (TODO: Change this), and a list of attributes.
data ReqChunk = RC 
  { _id        :: String
  , reqType    :: ReqType 
  , requires   :: Sentence
  , _refName   :: RefName -- HACK for refs?
  , _atts      :: Attributes
  }
  
instance HasUID ReqChunk where uid f (RC a b c d e) = fmap (\x -> RC x b c d e) (f a)
instance HasAttributes ReqChunk where attributes f (RC a b c d e) = fmap (\x -> RC a b c d x) (f e)
instance Eq ReqChunk where a == b = a ^. uid == b ^. uid

-- | Smart constructor for requirement chunks (should not be exported)
rc :: String -> ReqType -> Sentence -> RefName -> Attributes -> ReqChunk
rc = RC

rc' :: ReqChunk -> Sentence -> ReqChunk
rc' r s = set attributes (shortname s : (r ^. attributes)) r

frc, nfrc :: String -> Sentence -> RefName -> Attributes -> ReqChunk
-- | Smart constructor for functional requirement chunks.
frc i = rc i FR

-- | Smart constructor for non-functional requirement chunks.
nfrc i = rc i NFR
