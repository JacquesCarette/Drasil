module Database.Drasil.Dump where

import Language.Drasil (UID, HasUID(..))
import Database.Drasil.ChunkDB (conceptinsTable, theoryModelTable, gendefTable,
  insmodelTable, dataDefnTable, unitTable, citationTable, UMap,
  ChunkDB(symbolTable))

import Data.Map.Strict (Map, insert)
import qualified Data.Map.Strict as SM

import Control.Lens ((^.))

type ChunkType = String
type DumpedChunkDB = Map ChunkType [UID]

umapDump :: HasUID a => UMap a -> [UID]
umapDump = map ((^. uid) . fst) . SM.elems

dumpChunkDB :: ChunkDB -> DumpedChunkDB
dumpChunkDB cdb = 
      insert "symbols" (umapDump $ symbolTable cdb)
    $ insert "concepts" (umapDump $ cdb ^. conceptinsTable)
    $ insert "units" (umapDump $ cdb ^. unitTable)
    $ insert "dataDefinitions" (umapDump $ cdb ^. dataDefnTable)
    $ insert "instanceModels" (umapDump $ cdb ^. insmodelTable)
    $ insert "generalDefinitions" (umapDump $ cdb ^. gendefTable)
    $ insert "theoryModels" (umapDump $ cdb ^. theoryModelTable)
    $ insert "conceptInstances" (umapDump $ cdb ^. conceptinsTable)
    $ insert "citations" (umapDump $ cdb ^. citationTable)
      mempty
