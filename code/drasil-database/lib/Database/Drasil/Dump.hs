{-# LANGUAGE TypeApplications #-}
module Database.Drasil.Dump where

import Language.Drasil (UID, HasUID(..))
import Database.Drasil.ChunkDB

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as SM

import Control.Lens ((^.))

type ChunkType = String
type DumpedChunkDB = Map ChunkType [UID]

umapDump :: HasUID a => UMap a -> [UID]
umapDump = map ((^. uid) . fst) . SM.elems

dumpChunkDB :: ChunkDB -> DumpedChunkDB
dumpChunkDB cdb = SM.fromList $ map (\ty -> (show ty, findAllUIDs ty cdb)) $ typesRegistered cdb
