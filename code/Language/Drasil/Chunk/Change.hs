module Language.Drasil.Chunk.Change 
  ( Change(..), ChngType(..)
  , lc, ulc--, chc'
  ) where

import Language.Drasil.Classes (HasUID(uid), HasAttributes(attributes), HasShortName)
import Language.Drasil.Chunk.Attribute.Core(Attributes)
import Language.Drasil.Chunk.Attribute(shortname)
import Language.Drasil.Spec (Sentence, RefName)

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
  , _refName :: RefName -- HACK for refs?
  , _atts    :: Attributes
  }
  
instance HasUID        Change where uid f (ChC a b c d e) = fmap (\x -> ChC x b c d e) (f a)
instance HasAttributes Change where attributes f (ChC a b c d e) = fmap (\x -> ChC a b c d x) (f e)
instance HasShortName  Change where
instance Eq            Change where a == b = a ^. uid == b ^. uid

-- | Smart constructor for requirement chunks (should not be exported)
chc :: String -> ChngType -> Sentence -> RefName -> Attributes -> Change
chc = ChC

--chc' :: Change -> Sentence -> Change
--chc' c s = set attributes ([shortname s] ++ (c ^. attributes)) c

lc, ulc :: String -> Sentence -> RefName -> Attributes -> Change
-- | Smart constructor for functional requirement chunks.
lc i = chc i Likely

-- | Smart constructor for non-functional requirement chunks.
ulc i = chc i Unlikely
