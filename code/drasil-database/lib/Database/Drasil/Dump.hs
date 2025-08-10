{-# LANGUAGE TypeApplications #-}
module Database.Drasil.Dump where

import Language.Drasil (UID, HasUID(..))
import Database.Drasil.ChunkDB

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as SM

import Control.Lens ((^.))
import Drasil.Database.Chunk (Chunk)
import Data.Typeable (typeRep, Proxy (..))

type ChunkType = String
type DumpedChunkDB = Map ChunkType [UID]

umapDump :: HasUID a => UMap a -> [UID]
umapDump = map ((^. uid) . fst) . SM.elems

dumpChunkDB :: ChunkDB -> DumpedChunkDB
dumpChunkDB cdb = 
      SM.insert "chunks" (map ((^. uid) :: Chunk -> UID) (findAll (typeRep (Proxy @Chunk)) cdb))
      mempty
