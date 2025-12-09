module Drasil.Database.Dump (
    ChunkType, DumpedChunkDB, dumpChunkDB
) where

import Drasil.Database.UID (UID)
import Drasil.Database.ChunkDB (findAll', typesRegistered, ChunkDB)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as SM

type ChunkType = String

-- | A dumped representation of a 'ChunkDB', mapping chunk types ('String's) to
-- lists of 'UID's.
type DumpedChunkDB = Map ChunkType [UID]

dumpChunkDB :: ChunkDB -> DumpedChunkDB
dumpChunkDB cdb = SM.fromList $ map (\ty -> (show ty, findAll' ty cdb)) (typesRegistered cdb)
