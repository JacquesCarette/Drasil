module MetaDatabase.Drasil.ChunkDB (
    basisCDB
) where

import Database.Drasil (ChunkDB (..), termMap, conceptMap, unitMap)
import Language.Drasil (IdeaDict, nw)
import Data.Drasil.Concepts.Documentation (doccon, doccon', srsDomains)
import Data.Drasil.Software.Products (prodtcon)
import Data.Drasil.Concepts.Education (educon)
import Data.Drasil.Concepts.Computation (compcon, algorithm)
import Data.Drasil.Concepts.Software (errMsg, program)
import Data.Drasil.Concepts.Math (mathcon)

import qualified Data.Map as Map (empty)
import Data.Drasil.SI_Units (siUnits)

basisIdeaDicts :: [IdeaDict]
basisIdeaDicts =
  -- Actual IdeaDicts
  doccon ++ prodtcon ++ educon ++ compcon ++
  -- CIs
  map nw doccon' ++
  -- ConceptChunks
  map nw [algorithm, errMsg, program] ++ map nw mathcon

basisCDB :: ChunkDB
basisCDB =
  CDB {
    -- CHUNKS
    symbolTable           = Map.empty,
    termTable             = termMap basisIdeaDicts,
    conceptChunkTable     = conceptMap srsDomains,
    _unitTable            = unitMap siUnits,
    _dataDefnTable        = Map.empty,
    _insmodelTable        = Map.empty,
    _gendefTable          = Map.empty,
    _theoryModelTable     = Map.empty,
    _conceptinsTable      = Map.empty,
    _citationTable        = Map.empty, 
    -- NOT CHUNKS
    _labelledcontentTable = Map.empty,
    _traceTable           = Map.empty,
    _refbyTable           = Map.empty,
    _refTable             = Map.empty
  }