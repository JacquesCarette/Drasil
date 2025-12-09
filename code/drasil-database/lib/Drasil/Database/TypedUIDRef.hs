module Drasil.Database.TypedUIDRef (
  TypedUIDRef, mkRef,
  typedFind, typedFindOrErr,
) where

import Control.Lens ((^.))
import Data.Maybe (fromMaybe)

import Drasil.Database.Chunk (IsChunk)
import Drasil.Database.ChunkDB (ChunkDB, find)
import Drasil.Database.UID (HasUID(..), UID)

-- | 'UID' references that contain information about the type of data the 'UID'
-- refers to, useful for type-safe dereferencing.
newtype TypedUIDRef typ = TypedUIDRef UID

-- | Create a 'TypedUIDRef' to a chunk.
mkRef :: IsChunk t => t -> TypedUIDRef t
mkRef t = TypedUIDRef $ t ^. uid

-- | Find a chunk by its typed UID reference.
typedFind :: IsChunk t => TypedUIDRef t -> ChunkDB -> Maybe t
typedFind (TypedUIDRef u) = find u

-- | Find a chunk by its typed UID reference, erroring if not found.
typedFindOrErr :: IsChunk t => TypedUIDRef t -> ChunkDB -> t
typedFindOrErr tu cdb = fromMaybe (error "Typed UID dereference failed.") (typedFind tu cdb)
