{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}

module Drasil.Database.Chunk
  ( Chunk,
    IsChunk,
    HasChunkRefs (..),
    mkChunk, -- FIXME: mkChunk should not be exported but is temporarily because this module is NOT in `drasil-database`
    unChunk,
    chunkType,
  )
where

import Control.Lens ((^.), to, Getter)
import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, cast, typeOf, typeRep)
import qualified Data.Set as S

import Drasil.Database.UID (HasUID (..), UID)

-- | All chunks should expose what chunks they reference/rely on, so that we can
-- test 'ChunkDB's to ensure all presupposed chunks are already registered.
class HasChunkRefs a where
  chunkRefs :: a -> S.Set UID

-- | Constraint for anything that may be considered a valid chunk type.
type IsChunk a = (HasUID a, HasChunkRefs a, Typeable a)

-- | A piece of reusable knowledge, with an internal identifier ('UID'),
-- possibly dependant on other chunks.
data Chunk = forall a. IsChunk a => Chunk a

instance Eq Chunk where
  (==) :: Chunk -> Chunk -> Bool
  l == r = l ^. uid == r ^. uid

instance HasUID Chunk where
  uid :: Getter Chunk UID
  uid = to (\(Chunk c) -> c ^. uid)

instance HasChunkRefs Chunk where
  chunkRefs :: Chunk -> S.Set UID
  chunkRefs (Chunk c) = chunkRefs c

-- | Create a 'Chunk', ensuring that 'Chunk's are never placed within 'Chunk's.
mkChunk :: IsChunk a => a -> Chunk
mkChunk a
  | typeOf a == typeRep (Proxy @Chunk) = error "Cannot place a Chunk inside of a Chunk"
  | otherwise = Chunk a

-- | "Open" chunks, casting them to a specific type.
unChunk :: Typeable a => Chunk -> Maybe a
unChunk (Chunk c) = cast c

-- | Ask a 'Chunk' for the type of data it codifies.
chunkType :: Chunk -> TypeRep
chunkType (Chunk c) = typeOf c
