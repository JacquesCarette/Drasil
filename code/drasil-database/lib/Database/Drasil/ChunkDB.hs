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
  insert, insertAll,
  -- * Temporary functions
  insertAllOutOfOrder11,
) where

import Control.Lens ((^.))
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, typeOf, typeRep)

-- NOTE: Strictness is important for (a) performance, (b) space leaks, (c)
-- avoiding chunk dependancy cycles and (d) ensuring operation consistency with
-- other databases.
import qualified Data.Map.Strict as M

import qualified Data.Set as S

import Drasil.Database.Chunk (Chunk, HasChunkRefs(chunkRefs), IsChunk,
  mkChunk, unChunk, chunkType)
import Language.Drasil (HasUID(..), UID)
import Utils.Drasil (invert)

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
}

-- | An empty 'ChunkDB'.
empty :: ChunkDB
empty = ChunkDB M.empty M.empty

-- | Create a 'ChunkDB' from a list of chunks. This will insert all chunks into
-- the database from the list, from left to right.
fromList :: IsChunk a => [a] -> ChunkDB
fromList = flip insertAll empty

-- | Query the 'ChunkDB' for all registered chunks (by their 'UID's).
registered :: ChunkDB -> [UID]
registered cdb = M.keys (chunkTable cdb)

-- | Check if a 'UID' is registered in the 'ChunkDB'.
isRegistered :: UID -> ChunkDB -> Bool
isRegistered u cdb = M.member u (chunkTable cdb)

-- | Enumerate all types registered in the 'ChunkDB'.
typesRegistered :: ChunkDB -> [TypeRep]
typesRegistered cdb = M.keys (chunkTypeTable cdb)

-- | Get the number of chunks registered in the 'ChunkDB'.
size :: ChunkDB -> Int
size cdb = M.size (chunkTable cdb)

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
findAll cdb = maybe [] (mapMaybe unChunk) $ M.lookup tr (chunkTypeTable cdb)
  where
    tr = typeRep (Proxy :: Proxy a)

-- | Find all chunks of a specific type in the 'ChunkDB', returning their 'UID's
-- rather than the chunks themselves.
findAll' :: TypeRep -> ChunkDB -> [UID]
findAll' tr cdb = maybe [] (map (^. uid)) $ M.lookup tr (chunkTypeTable cdb)

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

-- | Internal function for updating a chunk's list of depndant chunks (i.e.,
-- chunks that reference it).
insertRefsExpectingExistence :: [UID] -> UID -> ChunkByUID -> ChunkByUID
insertRefsExpectingExistence newDpdnts depdncy cbu =
  case M.insertLookupWithKey (\_ _ (c, dpdnts) -> (c, newDpdnts ++ dpdnts)) depdncy (undefined, []) cbu of
    (Just _, cbu') -> cbu' -- If the chunk is already registered, we just updated its dependants, and everything is fine.
    (Nothing, _) -> -- But if no data was found, then we have a problem: the chunk we are inserting depends on a chunk that does not exist.
      error $ "Chunk dependancy is missing for `" ++ show newDpdnts ++ "`. Missing: `" ++ show depdncy ++ "`."

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
    chunkTable'' = foldr (insertRefsExpectingExistence [c' ^. uid]) chunkTable'
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
            then error ("ERROR! Attempting to insert duplicate chunk: `" ++ show (c ^. uid) ++ "` :: " ++ show x) cdb''
            else error $ "ERROR! Attempting to overwrite a chunk (`" ++ show (c ^. uid) ++ "` :: `" ++ show x ++ "`) with a chunk of a different type: `" ++ show (typeOf c) ++ "`"
  | otherwise = insert0 cdb c

-- | Insert a list of chunks into a 'ChunkDB'.
insertAll :: IsChunk a => [a] -> ChunkDB -> ChunkDB
insertAll as cdb = foldl' (flip insert) cdb as

--------------------------------------------------------------------------------
-- Temporary functions for working with non-chunk tables
--
-- Everything below is temporary and should be removed once the LabelledContent
-- and Reference chunks are properly implemented and the "chunk refs" tables are
-- built properly (i.e., using the `HasChunkRefs` typeclass).
--------------------------------------------------------------------------------

-- | Insert 11 lists of /unique/ chunk types into a 'ChunkDB', assuming the
-- input 'ChunkDB' does not already contain any of the chunks from the chunk
-- lists.
insertAllOutOfOrder11 ::
  (IsChunk a, IsChunk b, IsChunk c, IsChunk d, IsChunk e,
   IsChunk f, IsChunk g, IsChunk h, IsChunk i, IsChunk j,
   IsChunk k) =>
   ChunkDB ->
   [a] -> [b] -> [c] -> [d] -> [e] ->
   [f] -> [g] -> [h] -> [i] -> [j] ->
   [k] -> ChunkDB
insertAllOutOfOrder11 strtr as bs cs ds es fs gs hs is js ks =
  let
    -- Box all of our chunks
    as' = map mkChunk as
    bs' = map mkChunk bs
    cs' = map mkChunk cs
    ds' = map mkChunk ds
    es' = map mkChunk es
    fs' = map mkChunk fs
    gs' = map mkChunk gs
    hs' = map mkChunk hs
    is' = map mkChunk is
    js' = map mkChunk js
    ks' = map mkChunk ks

    -- Put all of our chunks in a list of lists, with each list carrying a
    -- unique type of chunk, filtering out empty lists
    altogether = filter (not . null)
                  [as', bs', cs', ds', es', fs', gs', hs', is', js', ks']
    calt = concat altogether

    -- Calculate what chunks are depended on (i.e., UID -> Dependants)
    chDpdts = invert $ M.fromList $ map (\c -> (c ^. uid, S.toList $ chunkRefs c)) calt

    -- Insert all incoming chunks with the existing chunk table, asserting that
    -- none of the inserted chunks were already inserted.
    chTab = M.unionWith
      (\(x, _) _ -> error $ "duplicate chunk found in mass insertion: " ++ show (x ^. uid))
      (chunkTable strtr)
      (M.fromList $ map (\c -> (c ^. uid, (c, []))) calt)

    -- Merge the chunk-deps table with that existing chunks table
    chTabWDeps = M.foldlWithKey'
      (\acc k dpdts -> insertRefsExpectingExistence dpdts k acc) chTab chDpdts

    -- Create the list of new chunk types and add them to the previous list of chunk types
    chTys = M.fromList (map (\chs -> (chunkType $ head chs, chs)) altogether)
    chTT = M.unionWith (++) (chunkTypeTable strtr) chTys
  in
    -- Create the updated chunk database, adding the LCs and Rs, ignoring their dependencies.
    strtr { chunkTable = chTabWDeps
          , chunkTypeTable = chTT }
