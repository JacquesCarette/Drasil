module Database.Drasil.TypedUIDRef
  ( TypedUIDRef,
    mkRef,
    typedFind,
    typedFindOrErr,
  )
where

import Data.Maybe (fromMaybe)
import Drasil.Database.Chunk (IsChunk)
import Drasil.Database.TypedUIDRef (TypedUIDRef(..), mkRef)
import Database.Drasil.ChunkDB (ChunkDB, find)

typedFind :: IsChunk t => TypedUIDRef t -> ChunkDB -> Maybe t
typedFind (TypedUIDRef u) = find u

typedFindOrErr :: IsChunk t => TypedUIDRef t -> ChunkDB -> t
typedFindOrErr tu cdb = fromMaybe (error "Typed UID dereference failed.") (typedFind tu cdb)
