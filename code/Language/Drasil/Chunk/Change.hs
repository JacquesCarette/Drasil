module Language.Drasil.Chunk.Change 
  ( Change(..), ChngType(..)
  , lc, ulc
  ) where

import Language.Drasil.Classes (HasUID(uid),HasAttributes(attributes))
import Language.Drasil.Chunk.Attribute.Core(Attributes)
import Language.Drasil.Chunk.Attribute.ShortName
import Language.Drasil.Chunk.Attribute(shortname')
import Language.Drasil.Spec (Sentence)

import Control.Lens (set, (^.))

-- FIXME: We need a better way to capture change information. Sentences
-- are dead information, and larger structures (like Contents) are display-specific.
-- For now, using sentences to test.

-- | What type of change are we dealing with?
data ChngType = Likely -- ^ Likely Change
              | Unlikely -- ^ Unlikely Change
  deriving Eq
  
instance Show ChngType where
  show Likely = "LC"
  show Unlikely = "UC"

-- | Requirement chunk type. Has an id, the type of requirement
-- (Functional/Non-Functional) from 'ChngType', a sentence describing what is
-- required (TODO: Change this), and a list of attributes.
data Change = ChC 
  { _id      :: String
  , chngType :: ChngType 
  , chng     :: Sentence
  , _refName :: ShortName
  , _atts    :: Attributes
  }
  
instance HasUID        Change where uid f (ChC a b c d e) = fmap (\x -> ChC x b c d e) (f a)
instance HasAttributes Change where attributes f (ChC a b c d e) = fmap (\x -> ChC a b c d x) (f e)
instance Eq            Change where a == b = a ^. uid == b ^. uid
instance HasShortName  Change where
  shortname (ChC _ _ _ sn _)     = sn

-- | Smart constructor for requirement chunks (should not be exported)
chc :: String -> ChngType -> Sentence -> ShortName -> Attributes -> Change
chc = ChC

lc, ulc :: String -> Sentence -> ShortName -> Attributes -> Change
-- | Smart constructor for functional requirement chunks.
lc i = chc i Likely

-- | Smart constructor for non-functional requirement chunks.
ulc i = chc i Unlikely
