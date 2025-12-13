module Drasil.Database.UIDRef (
  -- * 'UID' References
  TypedUIDRef, mkTypedRef,
  typedDeref, typedDerefOrErr,
  UIDRef, mkRef,
  deref, derefOrErr
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
newtype TypedUIDRef typ = TypedUIDRef UID

instance HasChunkRefs (TypedUIDRef t) where
  -- | A 'TypedUIDRef t' carries a 'UID' referring to a chunk of type 't'.
  chunkRefs (TypedUIDRef u) = S.singleton u
  {-# INLINABLE chunkRefs #-}

-- | Create a 'TypedUIDRef' to a chunk.
mkTypedRef :: IsChunk t => t -> TypedUIDRef t
mkTypedRef = TypedUIDRef . (^. uid)

-- | Find a chunk by a 'TypedUIDRef'.
typedDeref :: IsChunk t => TypedUIDRef t -> ChunkDB -> Maybe t
typedDeref (TypedUIDRef u) = find u

-- | Find a chunk by a 'TypedUIDRef', erroring if not found.
typedDerefOrErr :: IsChunk t => TypedUIDRef t -> ChunkDB -> t
typedDerefOrErr tu cdb = fromMaybe (error "Typed UID dereference failed.") (typedDeref tu cdb)

-- | A variant of 'TypedUIDRef' without type information about the chunk being
-- referred to.
newtype UIDRef = UIDRef UID

instance HasChunkRefs UIDRef where
  -- | A 'UIDRef t' carries a 'UID' referring to another chunk.
  chunkRefs (UIDRef u) = S.singleton u
  {-# INLINABLE chunkRefs #-}

-- | Create a 'UIDRef' to a chunk.
mkRef :: IsChunk t => t -> UIDRef
mkRef = UIDRef . (^. uid)

-- | Find a chunk by its 'UIDRef'.
deref :: IsChunk t => UIDRef -> ChunkDB -> Maybe t
deref (UIDRef u) = find u

-- | Find a chunk by its 'UIDRef', erroring if not found.
derefOrErr :: IsChunk t => UIDRef -> ChunkDB -> t
derefOrErr tu cdb = fromMaybe (error "Untyped UID dereference failed.") (deref tu cdb)
