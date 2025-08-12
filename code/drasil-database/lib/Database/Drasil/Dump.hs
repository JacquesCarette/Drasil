module Database.Drasil.Dump where

import Language.Drasil (UID, HasUID(..))
import Database.Drasil.ChunkDB

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as SM

import Control.Lens ((^.))

type ChunkType = String

-- | A dumped representation of a 'ChunkDB', mapping chunk types ('String's) to
-- lists of 'UID's.
type DumpedChunkDB = Map ChunkType [UID]

dumpChunkDB :: ChunkDB -> DumpedChunkDB
dumpChunkDB cdb = SM.fromList $ map (\ty -> (show ty, findAll' ty cdb)) (typesRegistered cdb)
