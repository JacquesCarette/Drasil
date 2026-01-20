module Drasil.Database.UIDRef (
  -- * 'UID' References
  UIDRef, hide, unhide, unhideOrErr, uidRef, unRef,
  UnitypedUIDRef, hideUni, unhideUni, unhideUniOrErr
) where

import Control.Lens ((^.))
import Data.Maybe (fromMaybe)

import Drasil.Database.Chunk (IsChunk, HasChunkRefs (..))
import Drasil.Database.ChunkDB (ChunkDB, find)
import Drasil.Database.UID (HasUID(..), UID)
import qualified Data.Set as S (singleton)

-- | A reference to another chunk through its 'UID', with a type variable to
-- retain information about the original chunk's type, useful for type-safe
-- dereferencing.
newtype UIDRef typ = UIDRef UID

instance HasChunkRefs (UIDRef t) where
  -- | A 'UIDRef t' carries a 'UID' referring to a chunk of type 't'.
  chunkRefs (UIDRef u) = S.singleton u
  {-# INLINABLE chunkRefs #-}

-- | Create a 'UIDRef' to a chunk.
hide :: IsChunk t => t -> UIDRef t
hide = UIDRef . (^. uid)

-- | Create a 'UIDRef' from a raw 'UID'.
uidRef :: UID -> UIDRef t
uidRef = UIDRef

-- | Extract the 'UID' from a 'UIDRef'.
unRef :: UIDRef t -> UID
unRef (UIDRef u) = u

-- | Find a chunk by a 'UIDRef'.
unhide :: IsChunk t => UIDRef t -> ChunkDB -> Maybe t
unhide (UIDRef u) = find u

-- | Find a chunk by a 'UIDRef', erroring if not found.
unhideOrErr :: IsChunk t => UIDRef t -> ChunkDB -> t
unhideOrErr tu cdb = fromMaybe (error "Typed UID dereference failed.") (unhide tu cdb)

-- | A variant of 'UIDRef' without type information about the chunk being
-- referred to, effectively treating chunks as being "unityped."
newtype UnitypedUIDRef = UnitypedUIDRef UID

instance HasChunkRefs UnitypedUIDRef where
  -- | A 'UnitypedUIDRef t' carries a 'UID' referring to another chunk.
  chunkRefs (UnitypedUIDRef u) = S.singleton u
  {-# INLINABLE chunkRefs #-}

-- | Create a 'UnitypedUIDRef' to a chunk.
hideUni :: IsChunk t => t -> UnitypedUIDRef
hideUni = UnitypedUIDRef . (^. uid)

-- | Find a chunk by its 'UnitypedUIDRef'.
unhideUni :: IsChunk t => UnitypedUIDRef -> ChunkDB -> Maybe t
unhideUni (UnitypedUIDRef u) = find u

-- | Find a chunk by its 'UnitypedUIDRef', erroring if not found.
unhideUniOrErr :: IsChunk t => UnitypedUIDRef -> ChunkDB -> t
unhideUniOrErr tu cdb = fromMaybe (error "Untyped UID dereference failed.") (unhideUni tu cdb)
