module Language.Drasil.Chunk.Change 
  ( Change(..), ChngType(..)
  , lc, ulc, chc'
  ) where

import Language.Drasil.Classes (HasUID(uid), HasAttributes(attributes),
  HasShortName(shortname))
import Language.Drasil.Chunk.Attribute.Core(Attributes)
import Language.Drasil.Spec (Sentence)
import Language.Drasil.Chunk.Attribute.ShortName
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
  , _refName :: ShortNm
  , _atts    :: Attributes
  }
  
instance HasUID        Change where uid f (ChC a b c d e) = fmap (\x -> ChC x b c d e) (f a)
instance HasAttributes Change where attributes f (ChC a b c d e) = fmap (\x -> ChC a b c d x) (f e)
instance HasShortName  Change where shortname f (ChC a b c d e) = fmap (\x -> ChC a b c x e) (f d)
instance Eq            Change where a == b = a ^. uid == b ^. uid

-- | Smart constructor for requirement chunks (should not be exported)
chc :: String -> ChngType -> Sentence -> ShortNm -> Attributes -> Change
chc = ChC

chc' :: Change -> Sentence -> Change
chc' c s = set attributes (c ^. attributes) c

lc, ulc :: String -> Sentence -> ShortNm -> Attributes -> Change
-- | Smart constructor for functional requirement chunks.
lc i = chc i Likely

-- | Smart constructor for non-functional requirement chunks.
ulc i = chc i Unlikely
