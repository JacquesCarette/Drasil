module Drasil.Database.TypedUIDRef
  ( TypedUIDRef(..), -- FIXME: do not export the constructor
    mkRef
  )
where

import Drasil.Database.Chunk (IsChunk)
import Drasil.Database.UID (HasUID (uid), UID)
import Control.Lens ((^.))

-- | 'UID' references that contain information about the type of data the 'UID'
-- refers to, useful for type-safe dereferencing.
newtype TypedUIDRef typ = TypedUIDRef UID

-- | Create a 'TypedUIDRef' to a chunk.
mkRef :: IsChunk t => t -> TypedUIDRef t
mkRef t = TypedUIDRef $ t ^. uid
