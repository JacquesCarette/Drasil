module Drasil.Database.InsertManyOutOfOrderHack (
  insertAllOutOfOrder12
) where

import Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Drasil.Database.Chunk (HasChunkRefs(chunkRefs), TypeableChunk, mkChunk,
  chunkType, Chunk)
import Drasil.Database.ChunkDB (ChunkDB, chunkTypeTable, chunkTable,
  insertRefsExpectingExistence, registered)
import Drasil.Database.Maps (invert)
import Drasil.Database.UID (HasUID(..), UID)
import Data.List (partition)
import Debug.Trace (trace)
import Data.Bifunctor (Bifunctor(first))

-- | Insert 12 lists of /unique/ chunk types into a 'ChunkDB', assuming the
-- input 'ChunkDB' does not already contain any of the chunks from the chunk
-- lists.
insertAllOutOfOrder12 ::
  (TypeableChunk a, TypeableChunk b, TypeableChunk c, TypeableChunk d,
   TypeableChunk e, TypeableChunk f, TypeableChunk g, TypeableChunk h,
   TypeableChunk i, TypeableChunk j, TypeableChunk k, TypeableChunk l) =>
   ChunkDB ->
   [a] -> [b] -> [c] -> [d] -> [e] ->
   [f] -> [g] -> [h] -> [i] -> [j] ->
   [k] -> [l] -> ChunkDB
insertAllOutOfOrder12 strtr as bs cs ds es fs gs hs is js ks ls =
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
    ls' = map mkChunk ls

    -- Put all of our chunks in a list of lists, with each list carrying a
    -- unique type of chunk, filtering out empty lists
    altogether = filter (not . null)
                  [as', bs', cs', ds', es', fs', gs', hs', is', js', ks', ls']
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

    -- Create the updated chunk database, adding the LCs and Rs, ignoring their dependencies.
    strtr' = strtr { chunkTable = chTabWDeps
                   , chunkTypeTable = chTT }

    calt' = map (\x -> (x, chunkRefs x)) $ concat altogether
    initKnowns = S.fromList $ registered strtr
    order = insertionOrder initKnowns calt'
  in
    trace ("one feasible order: " ++ show order) strtr'

insertionOrder :: S.Set UID -> [(Chunk, S.Set UID)] -> [[(UID, S.Set UID)]]
insertionOrder _ [] = []
insertionOrder knowns chkNDeps =
  let (nextAdditions, notYet) = partition (flip S.isSubsetOf knowns . snd) chkNDeps
  in case nextAdditions of
    [] -> error $ "bad insertion, missing chunks for: " ++ show (bimap (^. uid) (S.\\ knowns) <$> notYet)
    adds -> let knowns' = knowns `S.union` S.fromList ((^. uid) . fst <$> nextAdditions)
                subOrd = insertionOrder knowns' notYet
             in (first (^. uid) <$> adds) : subOrd
