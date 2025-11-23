module Drasil.Database.Dump where

import Drasil.Database.UID (UID)
import Database.Drasil.ChunkDB

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as SM

type ChunkType = String

-- | A dumped representation of a 'ChunkDB', mapping chunk types ('String's) to
-- lists of 'UID's.
type DumpedChunkDB = Map ChunkType [UID]

dumpChunkDB :: ChunkDB -> DumpedChunkDB
dumpChunkDB cdb = SM.fromList $ map (\ty -> (show ty, findAll' ty cdb)) (typesRegistered cdb)
