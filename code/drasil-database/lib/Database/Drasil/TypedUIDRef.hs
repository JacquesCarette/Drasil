module Drasil.Database.TypedUIDRef
  ( TypedUIDRef,
    mkRef,
    typedFind,
    typedFindOrErr,
  )
where

import Data.Maybe (fromMaybe)
import Database.Drasil.Chunk (IsChunk)
import Database.Drasil.ChunkDB (ChunkDB, find)
import Language.Drasil (HasUID (uid), UID)
import Control.Lens ((^.))

-- | 'TypedUIDRef' represents typed references to chunks using their 'UID' and
-- type.
newtype TypedUIDRef typ = TypedUIDRef UID

mkRef :: IsChunk t => t -> TypedUIDRef t
mkRef t = TypedUIDRef $ t ^. uid

typedFind :: IsChunk t => TypedUIDRef t -> ChunkDB -> Maybe t
typedFind (TypedUIDRef u) = find u

typedFindOrErr :: IsChunk t => TypedUIDRef t -> ChunkDB -> t
typedFindOrErr tu cdb = fromMaybe (error "Typed UID dereference failed.") (typedFind tu cdb)
