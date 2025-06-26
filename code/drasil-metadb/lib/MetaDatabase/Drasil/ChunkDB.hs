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

-- | The basic idea dicts that are used to construct the basis chunk database.
-- Every chunk added here is added to every new chunk database created that uses
--  the cdb constructor. This ensures that the information in these idea dicts
--  is always available in the chunk database.
basisIdeaDicts :: [IdeaDict]
basisIdeaDicts =
  -- Actual IdeaDicts
  doccon ++ prodtcon ++ educon ++ compcon ++
  -- CIs
  map nw doccon' ++
  -- ConceptChunks
  map nw [algorithm, errMsg, program] ++ map nw mathcon

-- | The basis chunk database, which contains the basic idea dicts, concept chunks,
--  and units that are used in all of the case studies. This database is then added
-- to all of the new chunk databases created using the cdb constructor.
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