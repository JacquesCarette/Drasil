module Drasil.Database.TypedUIDRef
  ( TypedUIDRef(..), -- FIXME: do not export the constructor
    mkRef
  )
where

import Drasil.Database.Chunk (IsChunk)
import Drasil.Database.UID (HasUID (uid), UID)
import Control.Lens ((^.))

-- | 'TypedUIDRef' represents typed references to chunks using their 'UID' and
-- type.
newtype TypedUIDRef typ = TypedUIDRef UID

mkRef :: IsChunk t => t -> TypedUIDRef t
mkRef t = TypedUIDRef $ t ^. uid
