-- | References that have extra information.
module Language.Drasil.Document.DecoratedReference (
  -- * Type
  DecRef,
  -- * Class
  HasDecRef(..),
  -- * Constructors
  dRef, dRefInfo,
  -- * Sentence
  decRefS
) where

import Control.Lens (Lens')

import Drasil.Database (UIDRef, HasChunkRefs(..), TypeableChunk, hide, raw)

import Language.Drasil.Sentence (Sentence(Ref, EmptyS), RefInfo(..))
import Language.Drasil.Document.Labels (HasRefAddress)
import Language.Drasil.Document.ShortName (HasShortName)

-- | For holding a reference to something that is decorated with extra
-- information (ex. page numbers, equation sources, etc.).
data DecRef where
  DR :: (TypeableChunk r, HasRefAddress r, HasShortName r) => UIDRef r -> RefInfo -> DecRef

-- | A class that contains a list of decorated references ('DecRef's).
class HasDecRef c where
  -- | Provides a 'Lens' to the 'DecRef's.
  getDecRefs :: Lens' c [DecRef]

-- | The referenced chunk, along with the chunk references of the extra
-- information.
instance HasChunkRefs DecRef where
  chunkRefs (DR r i) = chunkRefs r <> chunkRefs i

-- FIXME: This should also consider the types of the referenced chunks, not
-- only their 'UID's.
-- | Equal if both reference the same chunk.
instance Eq DecRef where DR a _ == DR b _ = raw a == raw b

-- | For creating a decorated reference ('DecRef') with extra reference information ('RefInfo').
dRefInfo :: (TypeableChunk r, HasRefAddress r, HasShortName r) => r -> RefInfo -> DecRef
dRefInfo = DR . hide

-- | Same as 'ref', but for 'DecRef' instead of 'Reference'.
dRef :: (TypeableChunk r, HasRefAddress r, HasShortName r) => r -> DecRef
dRef r = dRefInfo r None

-- | Translate a 'DecRef' into a human-readable 'Sentence'.
decRefS :: DecRef -> Sentence
decRefS (DR r i) = Ref (raw r) EmptyS i
