{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Database.Drasil.ChunkDB
  ( ChunkDB (refTable),
    UMap,
    empty,
    mkChunkDB,
    find,
    labelledcontentFind,
    refFind,
    findOrErr,
    findRefs,
    findRefsOrErr,
    findAll,
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
    refTable,
    traceTable,
    refbyLookup,
    traceLookup
  )
where

import Data.List (nub, (\\))
import qualified Data.Map.Strict as M -- NOTE: If we don't use 'Strict'ness here, it can cause funkiness! In particular, the `union` might not throw errors when it should!
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, typeOf, typeRep)

import Drasil.Database.Chunk (Chunk, HasChunkRefs (chunkRefs), IsChunk, mkChunk, unChunk)
import Language.Drasil (HasUID(..), UID, Quantity, MayHaveUnit(..), Idea (..),
  IdeaDict, Concept, ConceptChunk, IsUnit(..), UnitDefn,
  Reference, ConceptInstance, LabelledContent, Citation,
  nw, cw, unitWrapper, NP, NamedIdea(..), DefinedQuantityDict, dqdWr,
  Sentence, Definition (defn), ConceptDomain (cdom))
import Control.Lens ((^.))

type ReferredBy = [UID]

type ChunkByUID = M.Map UID (Chunk, ReferredBy)

type ChunksByTypeRep = M.Map TypeRep [Chunk]

type UMap a = M.Map UID (a, Int)

data ChunkDB = ChunkDB {
    chunkTable :: ChunkByUID
  , chunkTypeTable :: ChunksByTypeRep
  -- NOT CHUNKS
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

-- The TypeRep input and implicit type information is not optimal, but it is
-- required because we don't have access to the type information in the raw
-- expressions.
findAll :: Typeable a => TypeRep -> ChunkDB -> [a]
findAll tr cdb = maybe [] (mapMaybe unChunk) (M.lookup tr (chunkTypeTable cdb))

findRefs :: UID -> ChunkDB -> Maybe [UID]
findRefs u cdb = do
  (_, refs) <- M.lookup u (chunkTable cdb)
  Just refs

findRefsOrErr :: UID -> ChunkDB -> [UID]
findRefsOrErr u = fromMaybe (error $ "Failed to find references for unknown chunk " ++ show u) . find u

insert' :: IsChunk a => a -> ChunkDB -> ChunkDB
insert' = flip insert

insert :: IsChunk a => ChunkDB -> a -> ChunkDB
insert cdb c
  | typeOf c == typeRep (Proxy @ChunkDB) =
      error "Insertion of ChunkDBs in ChunkDBs is disallowed; please perform unions with them instead."
  | M.member (c ^. uid) (chunkTable cdb) =
      error $ "Attempting to register a chunk with an already registered UID; `" ++ show (c ^. uid) ++ "`"
  | otherwise = ChunkDB finalCu ctr' (labelledcontentTable cdb) (refTable cdb) (traceTable cdb) (refbyTable cdb)
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

insertAll :: IsChunk a => ChunkDB -> [a] -> ChunkDB
insertAll cdb l = foldr (flip insert) cdb (reverse l) -- note: the "reverse" is here to make insertions slightly more readable -- I don't want to use foldl (it seems many have complaints about it)

insertAll' :: IsChunk a => [a] -> ChunkDB -> ChunkDB
insertAll' = flip insertAll

insertAllOrIgnore :: IsChunk a => ChunkDB -> [a] -> ChunkDB
insertAllOrIgnore cdb = foldr (\next old -> if isRegistered (next ^. uid) cdb then old else insert old next) cdb

union :: ChunkDB -> ChunkDB -> ChunkDB
union cdb1 cdb2 = ChunkDB um trm lc ref trc refb
  where
    um :: ChunkByUID
    um = M.unionWithKey (\conflict _ _ -> error $ "Unioned ChunkDBs contains at least one UID collision; `" ++ show conflict ++ "`!") (chunkTable cdb1) (chunkTable cdb2)

    trm :: ChunksByTypeRep
    trm = M.unionWith (++) (chunkTypeTable cdb1) (chunkTypeTable cdb2)

    lc :: UMap LabelledContent
    lc = M.unionWithKey (\conflict _ _ -> error $ "Unioned ChunkDBs contains at least one LabelledContent UID collision; `" ++ show conflict ++ "`!") (labelledcontentTable cdb1) (labelledcontentTable cdb2)

    ref :: UMap Reference
    ref = M.unionWithKey (\conflict _ _ -> error $ "Unioned ChunkDBs contains at least one Reference UID collision; `" ++ show conflict ++ "`!") (refTable cdb1) (refTable cdb2)

    trc :: M.Map UID [UID]
    trc = M.unionWith (++) (traceTable cdb1) (traceTable cdb2)

    refb :: M.Map UID [UID]
    refb = M.unionWith (++) (refbyTable cdb1) (refbyTable cdb2)

-- {- FIXME: TO BE REWRITTEN, UNIMPORTANT -}
-- refbyTable :: ChunkDB -> M.Map UID [UID]
-- refbyTable (ChunkDB (x, _)) = M.map snd x



-- Note: The below should be a 'faster' variant of the above 'mkChunkDB', but it
-- is missing a few of the sanity checks on chunks, so it's not quite ready yet!

-- mkChunkDB :: [Chunk] -> ChunkDB
-- mkChunkDB cs = ChunkDB (cbu, csbtr)
--   where
--     cbu :: ChunkByUID -- TODO: build a proper reference list, post-facto
--     cbu =
--       M.fromListWithKey
--         ( \k (r1, _) (r2, _) ->
--             error $
--               "At least 2 chunks provided contain the same UID, `"
--                 ++ show k
--                 ++ "`, with types: "
--                 ++ show (chunkType r1)
--                 ++ " and "
--                 ++ show (chunkType r2)
--         )
--         $ map (\c -> (uid c, (c, []))) cs
--
--     trs :: [TypeRep]
--     trs = nub $ map chunkType cs
--
--     trcs :: [(TypeRep, [Chunk])]
--     trcs = map (\tr -> (tr, filter ((==) tr . chunkType) cs)) trs
--
--     csbtr :: ChunksByTypeRep
--     csbtr = M.fromList trcs
