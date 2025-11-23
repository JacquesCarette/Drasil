module Database.Drasil.TypedUIDRef (
  TypedUIDRef, mkRef,
  typedFind, typedFindOrErr,
) where

import Data.Maybe (fromMaybe)
import Drasil.Database.Chunk (IsChunk)
import Drasil.Database.TypedUIDRef (TypedUIDRef(..), mkRef)
import Drasil.Database.ChunkDB (ChunkDB, find)

-- | Find a chunk by its typed UID reference.
typedFind :: IsChunk t => TypedUIDRef t -> ChunkDB -> Maybe t
typedFind (TypedUIDRef u) = find u

-- | Find a chunk by its typed UID reference, erroring if not found.
typedFindOrErr :: IsChunk t => TypedUIDRef t -> ChunkDB -> t
typedFindOrErr tu cdb = fromMaybe (error "Typed UID dereference failed.") (typedFind tu cdb)
