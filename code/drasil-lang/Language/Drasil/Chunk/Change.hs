{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.Change
  ( Change(..), ChngType(..)
  , lc, ulc
  ) where

import Language.Drasil.Classes (HasUID(uid), HasLabel(getLabel))
import Language.Drasil.Chunk.ShortName (HasShortName(shortname))
import Language.Drasil.UID (UID)
import Language.Drasil.Spec (Sentence)
import Language.Drasil.Label.Core (Label)

import Control.Lens ((^.), makeLenses)

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
-- required (TODO: Change this), and a short name for reference display.
data Change = ChC
  { _cid      :: UID
  , chngType :: ChngType
  , chng     :: Sentence
  , _lb      :: Label
  }
makeLenses ''Change

instance HasUID        Change where uid = cid
instance Eq            Change where a == b = a ^. uid == b ^. uid
instance HasLabel      Change where getLabel = lb
instance HasShortName  Change where shortname = lb . shortname

-- | Smart constructor for requirement chunks (should not be exported)
chc :: String -> ChngType -> Sentence -> Label -> Change
chc = ChC

lc, ulc :: String -> Sentence -> Label -> Change
-- | Smart constructor for functional requirement chunks.
lc i = chc i Likely

-- | Smart constructor for non-functional requirement chunks.
ulc i = chc i Unlikely
