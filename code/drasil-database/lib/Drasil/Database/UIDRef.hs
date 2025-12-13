module Drasil.Database.UIDRef (
  TypedUIDRef, mkRef,
  typedFind, typedFindOrErr,
) where

import Control.Lens ((^.))
import Data.Maybe (fromMaybe)

import Drasil.Database.Chunk (IsChunk, HasChunkRefs (..))
import Drasil.Database.ChunkDB (ChunkDB, find)
import Drasil.Database.UID (HasUID(..), UID)
import qualified Data.Set as S (singleton)

-- | 'UID' references that contain information about the type of data the 'UID'
-- refers to, useful for type-safe dereferencing.
newtype TypedUIDRef typ = TypedUIDRef UID

instance HasChunkRefs (TypedUIDRef t) where
  -- | A 'TypedUIDRef t' carries a 'UID' referring to a chunk of type 't'.
  chunkRefs (TypedUIDRef u) = S.singleton u
  {-# INLINABLE chunkRefs #-}

-- | Create a 'TypedUIDRef' to a chunk.
mkRef :: IsChunk t => t -> TypedUIDRef t
mkRef t = TypedUIDRef $ t ^. uid

-- | Find a chunk by its typed UID reference.
typedFind :: IsChunk t => TypedUIDRef t -> ChunkDB -> Maybe t
typedFind (TypedUIDRef u) = find u

-- | Find a chunk by its typed UID reference, erroring if not found.
typedFindOrErr :: IsChunk t => TypedUIDRef t -> ChunkDB -> t
typedFindOrErr tu cdb = fromMaybe (error "Typed UID dereference failed.") (typedFind tu cdb)
