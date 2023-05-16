module Database.Drasil.Dump where

import Language.Drasil
import Database.Drasil.ChunkDB hiding (cdb)

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
    $ insert "terms" (umapDump $ termTable cdb)
    $ insert "concepts" (umapDump $ _conceptinsTable cdb)
    $ insert "units" (umapDump $ _unitTable cdb)
    $ insert "dataDefinitions" (umapDump $ _dataDefnTable cdb)
    $ insert "instanceModels" (umapDump $ _insmodelTable cdb)
    $ insert "generalDefinitions" (umapDump $ _gendefTable cdb)
    $ insert "theoryModels" (umapDump $ _theoryModelTable cdb)
    $ insert "conceptInstances" (umapDump $ _conceptinsTable cdb)
    $ insert "sections" (umapDump $ _sectionTable cdb)
    $ insert "labelledContent" (umapDump $ _labelledcontentTable cdb)
    $ insert "references" (umapDump $ _refTable cdb)
      mempty
