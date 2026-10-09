{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Drasil.Database.Chunk
  ( Chunk,
    IsChunk,
    TypeableChunk,
    HasChunkRefs (..),
    mkChunk, -- FIXME: mkChunk should not be exported but is temporarily because this module is NOT in `drasil-database`
    unChunk,
    chunkType,
  )
where

import Control.Lens (Getter, to, (^.))
import Data.Set qualified as S
import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, cast, typeOf, typeRep)
import Drasil.Database.UID (HasUID (..), UID)
import GHC.Generics
  ( Generic (Rep, from),
    Generically (..),
    K1 (..),
    M1 (..),
    U1,
    type (:*:) (..),
    type (:+:) (..),
  )

-- | Constraint for anything that may be considered a valid chunk type.
type IsChunk a = (HasUID a, HasChunkRefs a)

-- | Constraint for anything that may be considered a valid chunk type, and is
-- also 'Typeable'. Type-ability is necessary for storing/retrieving chunks
-- in/from the 'ChunkDB'.
--
-- 'TypeableChunk' is meant to be a purely /internal/ concept; users of Drasil
-- should only ever need to use 'IsChunk'. 'Typeable' is automatically derived
-- by GHC, but can cause issues with type inference if used directly in user
-- code, especially when involving parameterized chunk types.
type TypeableChunk a = (IsChunk a, Typeable a)

-- | A piece of reusable knowledge, with an internal identifier ('UID'),
-- possibly dependant on other chunks.
data Chunk = forall a. (TypeableChunk a) => Chunk a

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
mkChunk :: (TypeableChunk a) => a -> Chunk
mkChunk a
  | typeOf a == typeRep (Proxy @Chunk) = error "Cannot place a Chunk inside of a Chunk"
  | otherwise = Chunk a

-- | "Open" chunks, casting them to a specific type.
unChunk :: (Typeable a) => Chunk -> Maybe a
unChunk (Chunk c) = cast c

-- | Ask a 'Chunk' for the type of data it codifies.
chunkType :: Chunk -> TypeRep
chunkType (Chunk c) = typeOf c

-- | The majority of chunks will relate other chunks in some way. In other
-- words, the majority of our chunks *depend* on others. 'HasChunkRefs' is meant
-- as a way to capture what things a chunk *directly* refers to (i.e., depends
-- on directly).
class HasChunkRefs a where
  chunkRefs :: a -> S.Set UID

instance HasChunkRefs UID where
  -- \| 'UID's are meant to be "owned" (i.e., they are the unique identifier of
  -- the chunk being defined), not *carried as references to other chunks*.
  -- 'TypedUIDRef t' exists to be used as a *reference to another chunk of type
  -- 't'*. Therefore, `UID` has no chunk references.
  chunkRefs _ = S.empty
  {-# INLINEABLE chunkRefs #-}

instance HasChunkRefs Int where
  chunkRefs _ = S.empty
  {-# INLINEABLE chunkRefs #-}

instance HasChunkRefs Integer where
  chunkRefs _ = S.empty
  {-# INLINEABLE chunkRefs #-}

instance HasChunkRefs Double where
  chunkRefs _ = S.empty
  {-# INLINEABLE chunkRefs #-}

instance HasChunkRefs Bool where
  chunkRefs _ = S.empty
  {-# INLINEABLE chunkRefs #-}

instance HasChunkRefs Char where
  chunkRefs _ = S.empty
  {-# INLINEABLE chunkRefs #-}

-- NOTE: 'OVERLAPPING' instance here because [Char] is instantiated with
-- `HasChunkRefs [a]`, but very inefficient. We already know the result will be
-- empty.
instance {-# OVERLAPPING #-} HasChunkRefs String where
  chunkRefs _ = S.empty
  {-# INLINEABLE chunkRefs #-}

instance (HasChunkRefs a) => HasChunkRefs [a] where
  chunkRefs = S.unions . fmap chunkRefs
  {-# INLINEABLE chunkRefs #-}

instance (HasChunkRefs a) => HasChunkRefs (Maybe a) where
  chunkRefs Nothing = S.empty
  chunkRefs (Just v) = chunkRefs v
  {-# INLINEABLE chunkRefs #-}

instance (HasChunkRefs l, HasChunkRefs r) => HasChunkRefs (Either l r) where
  chunkRefs = either chunkRefs chunkRefs
  {-# INLINEABLE chunkRefs #-}

instance (HasChunkRefs l, HasChunkRefs r) => HasChunkRefs (l, r) where
  chunkRefs (l, r) = chunkRefs l `S.union` chunkRefs r
  {-# INLINEABLE chunkRefs #-}

instance (Generic a, GHasCRefs (Rep a)) => HasChunkRefs (Generically a) where
  chunkRefs (Generically a) = gChunkRefs $ from a
  {-# INLINEABLE chunkRefs #-}

class GHasCRefs f where
  gChunkRefs :: f p -> S.Set UID

-- Meta-information (constructors, selectors): pass through
instance (GHasCRefs f) => GHasCRefs (M1 i c f) where
  gChunkRefs (M1 x) = gChunkRefs x
  {-# INLINEABLE gChunkRefs #-}

-- Products: Union
instance (GHasCRefs a, GHasCRefs b) => GHasCRefs (a :*: b) where
  gChunkRefs (a :*: b) = gChunkRefs a `S.union` gChunkRefs b
  {-# INLINEABLE gChunkRefs #-}

-- Sums: Depends on variant
instance (GHasCRefs a, GHasCRefs b) => GHasCRefs (a :+: b) where
  gChunkRefs (L1 x) = gChunkRefs x
  gChunkRefs (R1 x) = gChunkRefs x
  {-# INLINEABLE gChunkRefs #-}

-- Fields: Delegate
instance (HasChunkRefs c) => GHasCRefs (K1 i c) where
  gChunkRefs (K1 x) = chunkRefs x
  {-# INLINEABLE gChunkRefs #-}

-- Unit: Nothing!
instance GHasCRefs U1 where
  gChunkRefs _ = S.empty
  {-# INLINEABLE gChunkRefs #-}
