{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Database.Drasil.ChunkDB
  ( ChunkDB (refTable),
    UMap,
    idMap,
    empty,
    mkChunkDB,
    find,
    labelledcontentFind,
    refFind,
    findOrErr,
    findRefs,
    findRefsOrErr,
    findAll,
    findAllUIDs,
    findUnused,
    insert,
    insert',
    insertAll,
    insertAll',
    insertAllOrIgnore,
    union,
    registered,
    isRegistered,
    typesRegistered,
    numRegistered,
    refbyTable, -- FIXME: This function should be re-examined. Some functions can probably be moved here!
    labelledcontentTable,
    traceTable,
    refbyLookup,
    traceLookup
  )
where

import Data.List (nub, (\\))
import qualified Data.Map.Strict as M -- NOTE: Using strict maps is important, else `union` might not throw errors when it should.
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, typeOf, typeRep, cast)

import Drasil.Database.Chunk (Chunk, HasChunkRefs (chunkRefs), IsChunk, mkChunk, unChunk, chunkType)
import Language.Drasil (HasUID(..), UID, LabelledContent, Reference)
import Control.Lens ((^.))

import Debug.Trace (trace)

import Utils.Drasil (errMsg, warnMsg)

type ReferredBy = [UID]

type ChunkByUID = M.Map UID (Chunk, ReferredBy)

type ChunksByTypeRep = M.Map TypeRep [Chunk]

type UMap a = M.Map UID (a, Int)

idMap :: HasUID a => [a] -> UMap a
idMap vals = M.fromList $ zipWith (\v i -> (v ^. uid, (v, i))) vals [0..]

data ChunkDB = ChunkDB {
    chunkTable     :: ChunkByUID
  , chunkTypeTable :: ChunksByTypeRep

  -- FIXME: All things below need to be rebuilt.
  , labelledcontentTable :: UMap LabelledContent -- TODO: LabelledContent needs to be rebuilt. See JacquesCarette/Drasil#4023.
  , refTable             :: UMap Reference -- TODO: References need to be rebuilt. See JacquesCarette/Drasil#4022.
  , traceTable           :: M.Map UID [UID]
  , refbyTable           :: M.Map UID [UID]
}

empty :: ChunkDB
empty = ChunkDB M.empty M.empty M.empty M.empty M.empty M.empty

mkChunkDB :: IsChunk a => [a] -> ChunkDB
mkChunkDB = insertAll empty

registered :: ChunkDB -> [UID]
registered cdb = M.keys (chunkTable cdb) ++ M.keys (labelledcontentTable cdb) ++ M.keys (refTable cdb)

isRegistered :: UID -> ChunkDB -> Bool
isRegistered u cdb = M.member u (chunkTable cdb) || M.member u (labelledcontentTable cdb) || M.member u (refTable cdb)

typesRegistered :: ChunkDB -> [TypeRep]
typesRegistered cdb = M.keys (chunkTypeTable cdb) ++ [typeRep (Proxy @LabelledContent), typeRep (Proxy @Reference)]

numRegistered :: ChunkDB -> Int
numRegistered cdb = M.size (chunkTable cdb) + M.size (labelledcontentTable cdb) + M.size (refTable cdb)

find :: Typeable a => UID -> ChunkDB -> Maybe a
find u cdb = do
  (c', _) <- M.lookup u (chunkTable cdb)
  unChunk c'

findTypeOf :: UID -> ChunkDB -> Maybe TypeRep
findTypeOf u cdb = chunkType . fst <$> M.lookup u (chunkTable cdb)

-- | Looks up a 'UID' in a 'UMap' table. If nothing is found, an error is thrown.
uMapLookup :: String -> String -> UID -> UMap a -> a
uMapLookup tys ms u t = getFM $ M.lookup u t
  where getFM = maybe (error $ tys ++ ": " ++ show u ++ " not found in " ++ ms) fst

labelledcontentFind :: UID -> ChunkDB -> LabelledContent
labelledcontentFind u cdb = uMapLookup "LabelledContent" "labelledcontentTable" u (labelledcontentTable cdb)

refFind :: UID -> ChunkDB -> Reference
refFind u cdb = uMapLookup "Reference" "refTable" u (refTable cdb)

refbyLookup :: UID -> M.Map UID [UID] -> [UID]
refbyLookup c = fromMaybe [] . M.lookup c

-- | Trace a 'UID' to related 'UID's.
traceLookup :: UID -> M.Map UID [UID] -> [UID]
traceLookup c = fromMaybe [] . M.lookup c

findOrErr :: Typeable a => UID -> ChunkDB -> a
findOrErr u = fromMaybe (error $ "Failed to find chunk " ++ show u) . find u

-- The explicit TypeRep input and implicit type information is not optimal, but
-- it is required because we don't have access to the type information in the
-- raw expressions. We need both data and type information to find the
-- appropriate chunks.
findAll :: Typeable a => TypeRep -> ChunkDB -> [a]
findAll tr cdb
  | tr == typeRep (Proxy @LabelledContent) =
      mapMaybe (cast . fst) $ M.elems $ labelledcontentTable cdb
  | tr == typeRep (Proxy @Reference) =
      mapMaybe (cast . fst) $ M.elems $ refTable cdb
  | otherwise =
      maybe [] (mapMaybe unChunk) $ M.lookup tr (chunkTypeTable cdb)

findAllUIDs :: TypeRep -> ChunkDB -> [UID]
findAllUIDs tr cdb
  | tr == typeRep (Proxy @LabelledContent) =
      M.keys $ labelledcontentTable cdb
  | tr == typeRep (Proxy @Reference) =
      M.keys $ refTable cdb
  | otherwise =
      maybe [] (map (^. uid)) $ M.lookup tr (chunkTypeTable cdb)

findRefs :: UID -> ChunkDB -> Maybe [UID]
findRefs u cdb = do
  (_, refs) <- M.lookup u (chunkTable cdb)
  Just refs

findRefsOrErr :: UID -> ChunkDB -> [UID]
findRefsOrErr u = fromMaybe (error $ "Failed to find references for unknown chunk " ++ show u) . find u

findUnused :: ChunkDB -> [UID]
findUnused = M.keys . M.filter (\(_, refs) -> null refs) . chunkTable

insert' :: IsChunk a => a -> ChunkDB -> ChunkDB
insert' = flip insert

insert0 :: IsChunk a => ChunkDB -> a -> ChunkDB
insert0 cdb c = cdb'
  where
    c' :: Chunk
    c' = mkChunk c

    cu' :: ChunkByUID
    cu' = M.insert (c ^. uid) (c', []) (chunkTable cdb) -- insert our chunk, it is not currently referred by anything.

    insertRefExpectingExistence :: UID -> ChunkByUID -> ChunkByUID
    insertRefExpectingExistence u cbu =
      if isJust prev
        then cbu'
        else error $ "Referred knowledge is missing for `" ++ show (c ^. uid) ++ "`; needs `" ++ show u ++ "`"
      where
        (prev, cbu') = M.insertLookupWithKey (\_ _ (rcc, rcref) -> (rcc, u : rcref)) u (undefined, []) cbu

    finalCu :: ChunkByUID
    finalCu = foldr insertRefExpectingExistence cu' $ nub (chunkRefs c) \\ [c ^. uid]

    ctr' :: ChunksByTypeRep
    ctr' = M.alter (Just . maybe [c'] (++ [c'])) (typeOf c) (chunkTypeTable cdb)

    cdb' :: ChunkDB
    cdb' = cdb {chunkTable = finalCu, chunkTypeTable = ctr'}

insert :: IsChunk a => ChunkDB -> a -> ChunkDB
insert cdb c
  | typeOf c == typeRep (Proxy @ChunkDB) =
      error "Insertion of ChunkDBs in ChunkDBs is disallowed; please perform unions with them instead."
  | (Just x) <- findTypeOf (c ^. uid) cdb =
      -- Overwrite: remove previous chunk from chunk refs and chunksByType tables before inserting new one.
      let prevChunk = fst $ fromMaybe (error "Internal error: chunk missing after findTypeOf succeeded") (M.lookup (c ^. uid) (chunkTable cdb))
          prevType  = chunkType prevChunk
          cu' = M.delete (c ^. uid) (chunkTable cdb)
          ctr' = M.adjust (filter (\c_ -> (c_ ^. uid) /= (c ^. uid))) prevType (chunkTypeTable cdb)
          cdb' = cdb { chunkTable = cu', chunkTypeTable = ctr' }
          cdb'' = insert0 cdb' c
      in if typeOf c == x
            then trace (warnMsg $ "WARNING! Overwriting `" ++ show (c ^. uid) ++ "` :: " ++ show x) cdb''
            else trace (errMsg $ "SUPER-MEGA-ULTRA-DELUXE-WARNING! Overwriting a chunk (`" ++ show (c ^. uid) ++ "` :: `" ++ show x ++ "`) with a chunk of a different type: `" ++ show (typeOf c) ++ "`") cdb''
  | otherwise = insert0 cdb c

insertAll :: IsChunk a => ChunkDB -> [a] -> ChunkDB
insertAll cdb l = foldr (flip insert) cdb (reverse l) -- note: the "reverse" is here to make insertions slightly more readable -- I don't want to use foldl (it seems many have complaints about it)

insertAll' :: IsChunk a => [a] -> ChunkDB -> ChunkDB
insertAll' = flip insertAll

insertAllOrIgnore :: IsChunk a => ChunkDB -> [a] -> ChunkDB
insertAllOrIgnore cdb = foldr (\next old -> if isRegistered (next ^. uid) cdb then old else insert old next) cdb

union :: ChunkDB -> ChunkDB -> ChunkDB
union cdb1 cdb2 = ChunkDB um' trm' lc' ref' trc' refb'
  where
    um' :: ChunkByUID
    um' = M.unionWithKey (\conflict _ _ -> error $ "Unioned ChunkDBs contains at least one UID collision; `" ++ show conflict ++ "`!") (chunkTable cdb1) (chunkTable cdb2)

    trm' :: ChunksByTypeRep
    trm' = M.unionWith (++) (chunkTypeTable cdb1) (chunkTypeTable cdb2)

    lc' :: UMap LabelledContent
    lc' = M.unionWithKey (\conflict _ _ -> error $ "Unioned ChunkDBs contains at least one LabelledContent UID collision; `" ++ show conflict ++ "`!") (labelledcontentTable cdb1) (labelledcontentTable cdb2)

    ref' :: UMap Reference
    ref' = M.unionWithKey (\conflict _ _ -> error $ "Unioned ChunkDBs contains at least one Reference UID collision; `" ++ show conflict ++ "`!") (refTable cdb1) (refTable cdb2)

    trc' :: M.Map UID [UID]
    trc' = M.unionWith (\l r -> nub $ l ++ r) (traceTable cdb1) (traceTable cdb2)

    refb' :: M.Map UID [UID]
    refb' = M.unionWith (\l r -> nub $ l ++ r) (refbyTable cdb1) (refbyTable cdb2)
