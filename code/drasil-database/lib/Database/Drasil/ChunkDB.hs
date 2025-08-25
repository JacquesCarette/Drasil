{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Drasil.ChunkDB (
  -- * Core database types and functions.
  ChunkDB,
  empty, fromList,
  registered, typesRegistered, size,
  isRegistered,
  findUnused,
  find, findOrErr,
  findAll, findAll',
  dependants, dependantsOrErr,
  findTypeOf,
  insert, insertAll, union,
  -- * Temporary functions for working with non-chunk tables
  UMap, idMap,
  refTable, refFind,
  labelledcontentTable, labelledcontentFind,
  refbyTable, refbyLookup,
  traceTable, traceLookup
) where

import Control.Lens ((^.))
import Data.Foldable (foldl')
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, typeOf, typeRep, cast)

-- NOTE: Debug.Trace should only be used for warnings and errors, not for
-- general logging, as it can affect program behavior in unexpected ways.
-- However, we (ab)use it here to provide *soft* warnings when overwriting
-- chunks in the database.
import Debug.Trace (trace)

-- NOTE: Strictness is important for (a) performance, (b) space leaks, (c)
-- avoiding chunk dependancy cycles and (d) ensuring operation consistency with
-- other databases.
import qualified Data.Map.Strict as M

import Drasil.Database.Chunk (Chunk, HasChunkRefs(chunkRefs), IsChunk,
  mkChunk, unChunk, chunkType)
import Language.Drasil (HasUID(..), UID, LabelledContent, Reference)

-- | A chunk that depends on another.
type Dependant = UID

-- | Mapping of 'UID's to 'Chunk's and their dependants.
type ChunkByUID = M.Map UID (Chunk, [Dependant])

-- | Mapping of chunk types to lists of instances of them (chunks).
type ChunksByTypeRep = M.Map TypeRep [Chunk]

-- | Drasil's knowledge database.
data ChunkDB = ChunkDB {
    chunkTable     :: ChunkByUID
  , chunkTypeTable :: ChunksByTypeRep

  -- FIXME: All things below need to be rebuilt!!

  -- FIXME: All code in this file contains hacks specifically for the old
  -- LabelledContent and Reference chunks. Once rebuilt, these chunks should not
  -- have a unique 'UID' and should be registered in the 'ChunkDB' like any
  -- other chunk.

  -- TODO: LabelledContent needs to be rebuilt. See JacquesCarette/Drasil#4023.
  , labelledcontentTable :: UMap LabelledContent
  -- TODO: References need to be rebuilt. See JacquesCarette/Drasil#4022.
  , refTable             :: UMap Reference
  , traceTable           :: M.Map UID [UID]
  , refbyTable           :: M.Map UID [UID]
}

-- | An empty 'ChunkDB'.
empty :: ChunkDB
empty = ChunkDB M.empty M.empty M.empty M.empty M.empty M.empty

-- | Create a 'ChunkDB' from a list of chunks. This will insert all chunks into
-- the database from the list, from left to right.
fromList :: IsChunk a => [a] -> ChunkDB
fromList = flip insertAll empty

-- | Query the 'ChunkDB' for all registered chunks (by their 'UID's).
registered :: ChunkDB -> [UID]
registered cdb =
     M.keys (chunkTable cdb)
  ++ M.keys (labelledcontentTable cdb)
  ++ M.keys (refTable cdb)

-- | Check if a 'UID' is registered in the 'ChunkDB'.
isRegistered :: UID -> ChunkDB -> Bool
isRegistered u cdb =
     M.member u (chunkTable cdb)
  || M.member u (labelledcontentTable cdb)
  || M.member u (refTable cdb)

-- | Enumerate all types registered in the 'ChunkDB'.
typesRegistered :: ChunkDB -> [TypeRep]
typesRegistered cdb =
    typeRep (Proxy @LabelledContent)
  : typeRep (Proxy @Reference)
  : M.keys (chunkTypeTable cdb)

-- | Get the number of chunks registered in the 'ChunkDB'.
size :: ChunkDB -> Int
size cdb =
    M.size (chunkTable cdb)
  + M.size (labelledcontentTable cdb)
  + M.size (refTable cdb)

-- | Filter the 'ChunkDB' for chunks that are not needed by any other chunks.
-- These are the only chunks that can safely be removed from the database,
-- though we do not include this functionality.
findUnused :: ChunkDB -> [UID]
findUnused = M.keys . M.filter (\(_, refs) -> null refs) . chunkTable

-- | Find a chunk by its 'UID' in the 'ChunkDB'.
find :: Typeable a => UID -> ChunkDB -> Maybe a
find u cdb = do
  (c', _) <- M.lookup u (chunkTable cdb)
  unChunk c'

-- | Find a chunk by its 'UID' in the 'ChunkDB', throwing a hard error if it is
-- not found.
findOrErr :: forall a. Typeable a => UID -> ChunkDB -> a
findOrErr u = fromMaybe (error $ "Failed to find chunk " ++ show u ++ " (expected type: " ++ show (typeRep $ Proxy @a) ++ ")") . find u

-- | Find all chunks of a specific type in the 'ChunkDB'.
findAll :: forall a. IsChunk a => ChunkDB -> [a]
findAll cdb
  | tr == typeRep (Proxy @LabelledContent) =
      mapMaybe (cast . fst) $ M.elems $ labelledcontentTable cdb
  | tr == typeRep (Proxy @Reference) =
      mapMaybe (cast . fst) $ M.elems $ refTable cdb
  | otherwise =
      maybe [] (mapMaybe unChunk) $ M.lookup tr (chunkTypeTable cdb)
  where
    tr = typeRep (Proxy :: Proxy a)

-- | Find all chunks of a specific type in the 'ChunkDB', returning their 'UID's
-- rather than the chunks themselves.
findAll' :: TypeRep -> ChunkDB -> [UID]
findAll' tr cdb
  | tr == typeRep (Proxy @LabelledContent) =
      M.keys $ labelledcontentTable cdb
  | tr == typeRep (Proxy @Reference) =
      M.keys $ refTable cdb
  | otherwise =
      maybe [] (map (^. uid)) $ M.lookup tr (chunkTypeTable cdb)

-- | Find all chunks that depend on a specific one.
dependants :: UID -> ChunkDB -> Maybe [UID]
dependants u cdb = do
  (_, refs) <- M.lookup u (chunkTable cdb)
  Just refs

-- | Find all chunks that depend on a specific one, throwing a hard error if the
-- dependency chunk is not found.
dependantsOrErr :: UID -> ChunkDB -> [UID]
dependantsOrErr u = fromMaybe (error $ "Failed to find references for unknown chunk " ++ show u) . find u

-- | Find the type of a chunk by its 'UID'.
findTypeOf :: UID -> ChunkDB -> Maybe TypeRep
findTypeOf u cdb = chunkType . fst <$> M.lookup u (chunkTable cdb)

-- | Internal function for inserting a dependancy of a chunk into the
-- dependancy's respective dependants list.
insertRefExpectingExistence :: UID -> UID -> ChunkByUID -> ChunkByUID
insertRefExpectingExistence depdnt depdncy cbu =
  case M.insertLookupWithKey (\_ _ (c, depdnts) -> (c, depdnt : depdnts)) depdncy (undefined, []) cbu of
    (Just _, cbu') -> cbu' -- If the chunk is already registered, we just updated its dependants, and everything is fine.
    (Nothing, _) -> -- But if no data was found, then we have a problem: the chunk we are inserting depends on a chunk that does not exist.
      error $ "Chunk dependancy is missing for `" ++ show depdnt ++ "`. Missing: `" ++ show depdncy ++ "`."

-- | Internal function to insert a chunk into the 'ChunkDB'. This function
-- assumes that the chunk is not already registered in the database, and quietly
-- break table synchronicity if it is.
insert0 :: IsChunk a => ChunkDB -> a -> ChunkDB
insert0 cdb c = cdb'
  where
    -- Box our chunk.
    c' = mkChunk c

    -- Insert our chunk, it is not currently depended on by anything.
    chunkTable' = M.insert (c ^. uid) (c', mempty) (chunkTable cdb)

    -- Capture all dependencies of this chunk.
    chunkTable'' = foldr (insertRefExpectingExistence $ c' ^. uid) chunkTable'
      $ chunkRefs c

    -- Add our chunk to its corresponding 'chunks by type' list.
    chunkTypeTable' = M.alter (Just . maybe [c'] (++ [c'])) (typeOf c) (chunkTypeTable cdb)

    -- Finally, build the updated database.
    cdb' = cdb { chunkTable = chunkTable'', chunkTypeTable = chunkTypeTable' }

-- | Insert a chunk into the 'ChunkDB' if it is sensibly to do so (i.e., does
-- not depend on itself and is not a 'ChunkDB'). We temporarily allow chunks to
-- overwrite other ones, but we warn when this happens.
insert :: IsChunk a => a -> ChunkDB -> ChunkDB
insert c cdb
  | c ^. uid `elem` chunkRefs c =
      error $ "Chunk `" ++ show (c ^. uid) ++ "` cannot reference itself as a dependancy."
  | typeOf c == typeRep (Proxy @ChunkDB) =
      error "Insertion of ChunkDBs in ChunkDBs is disallowed; please perform unions with them instead."
  | (Just x) <- findTypeOf (c ^. uid) cdb =
      -- Overwrite: remove previous chunk from chunk refs and chunksByType
      -- tables before inserting new one.
      let prevChunk = fst $ fromMaybe (error "Internal error: chunk missing after findTypeOf succeeded") (M.lookup (c ^. uid) (chunkTable cdb))
          prevType  = chunkType prevChunk
          cu' = M.delete (c ^. uid) (chunkTable cdb)
          ctr' = M.adjust (filter (\c_ -> (c_ ^. uid) /= (c ^. uid))) prevType (chunkTypeTable cdb)
          cdb' = cdb { chunkTable = cu', chunkTypeTable = ctr' }
          cdb'' = insert0 cdb' c
      in if typeOf c == x
            then trace ("WARNING! Overwriting `" ++ show (c ^. uid) ++ "` :: " ++ show x) cdb''
            else error $ "ERROR! Overwriting a chunk (`" ++ show (c ^. uid) ++ "` :: `" ++ show x ++ "`) with a chunk of a different type: `" ++ show (typeOf c) ++ "`"
  | otherwise = insert0 cdb c

-- | Insert a list of chunks into a 'ChunkDB'.
insertAll :: IsChunk a => [a] -> ChunkDB -> ChunkDB
insertAll as cdb = foldl' (flip insert) cdb as

-- | Union two 'ChunkDB's together, throwing an error for any 'UID' collisions.
union :: ChunkDB -> ChunkDB -> ChunkDB
union cdb1 cdb2 = ChunkDB um' trm' lc' ref' trc' refb'
  where
    um'   = M.unionWithKey (\conflict _ _ -> error $ "Unioned ChunkDBs contains at least one UID collision; `" ++ show conflict ++ "`!") (chunkTable cdb1) (chunkTable cdb2)
    trm'  = M.unionWith (++) (chunkTypeTable cdb1) (chunkTypeTable cdb2)
    lc'   = M.unionWithKey (\conflict _ _ -> error $ "Unioned ChunkDBs contains at least one LabelledContent UID collision; `" ++ show conflict ++ "`!") (labelledcontentTable cdb1) (labelledcontentTable cdb2)
    ref'  = M.unionWithKey (\conflict _ _ -> error $ "Unioned ChunkDBs contains at least one Reference UID collision; `" ++ show conflict ++ "`!") (refTable cdb1) (refTable cdb2)
    trc'  = M.unionWith (\l r -> nub $ l ++ r) (traceTable cdb1) (traceTable cdb2)
    refb' = M.unionWith (\l r -> nub $ l ++ r) (refbyTable cdb1) (refbyTable cdb2)

--------------------------------------------------------------------------------
-- Temporary functions for working with non-chunk tables
--
-- Everything below is temporary and should be removed once the LabelledContent
-- and Reference chunks are properly implemented and the "chunk refs" tables are
-- built properly (i.e., using the `HasChunkRefs` typeclass).
--------------------------------------------------------------------------------

-- | An ordered map based on 'Data.Map.Strict' for looking up chunks by their
-- 'UID's.
type UMap a = M.Map UID (a, Int)

-- | Create a 'UMap' from a list of chunks. Assumes that the leftmost chunk in
-- the list has index 0, increasing by 1 each step to the right.
idMap :: HasUID a => [a] -> UMap a
idMap vals = M.fromList $ zipWith (\v i -> (v ^. uid, (v, i))) vals [0..]

-- | Looks up a 'UID' in a 'UMap' table. If nothing is found, an error is thrown.
uMapLookup :: String -> String -> UID -> UMap a -> a
uMapLookup tys ms u t = getFM $ M.lookup u t
  where getFM = maybe (error $ tys ++ ": " ++ show u ++ " not found in " ++ ms) fst

-- | Find a 'LabelledContent' by its 'UID', throwing an error if it is not
-- found.
labelledcontentFind :: UID -> ChunkDB -> LabelledContent
labelledcontentFind u cdb = uMapLookup "LabelledContent" "labelledcontentTable" u (labelledcontentTable cdb)

-- | Find a 'Reference' by its 'UID', throwing an error if it is not found.
refFind :: UID -> ChunkDB -> Reference
refFind u cdb = uMapLookup "Reference" "refTable" u (refTable cdb)

-- | Find what chunks reference a given 'UID'.
refbyLookup :: UID -> M.Map UID [UID] -> [UID]
refbyLookup c = fromMaybe [] . M.lookup c

-- | Query a chunk for to what chunks it refers to.
traceLookup :: UID -> M.Map UID [UID] -> [UID]
traceLookup = refbyLookup -- Same implementation, just different name for code clarity.
