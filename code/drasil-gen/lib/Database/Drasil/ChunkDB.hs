module Database.Drasil.ChunkDB (
    basisCDB
) where

import Database.Drasil (ChunkDB (symbolTable, termTable, conceptChunkTable, _unitTable, _dataDefnTable,
  _insmodelTable, _gendefTable, _theoryModelTable, _conceptinsTable,
  _citationTable, _labelledcontentTable, _traceTable, _refbyTable, _refTable,
  CDB), termMap, conceptMap, unitMap)
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
  -- | Actual IdeaDicts
  --  * doccon - General documentation related IdeaDicts. Included in the basis
  --             as it is data which all the cases studies use and is not specific
  --             to a particular case study.
  --  * prodtcon - A list of a few IdeaDicts that are terms related to software products.
  --              This is included in the basis as it can be used to describe
  --              any software, which each of the case study examples produce.
  --              For example, one of the chunks, `sciCompS`, can be used to describe
  --              all of the software that Drasil generates, since it is all scientific
  --              computing software.
  --  * educon - IdeaDict chunks with information about education. Included in the basis
  --            as each case study should provide information about the expected users,
  --            which usually means describing the expected education level in related
  --            fields.
  --  * compcon - Computing related IdeaDicts. Since all of the case studies are
  --              concerned with software, this is included in the basis as the
  --              computing chunks are relevant to all of them.
  doccon ++ prodtcon ++ educon ++ compcon ++
  -- | CIs
  --  * doccon' - A list of CommonIdeas that are added for the same purpose as `doccon`.
  map nw doccon' ++
  -- ConceptChunks
  --  * mathcon - Math concepts. Math is widespread throughout all of the case studies
  --              and scientific computing software in general, so it is included
  --              in the basis.
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
    _unitTable            = unitMap siUnits, -- SI units are important to all case studies since they rely on physical quantities
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