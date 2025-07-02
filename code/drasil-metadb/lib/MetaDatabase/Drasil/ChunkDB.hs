module MetaDatabase.Drasil.ChunkDB (
  cdb
) where

import Database.Drasil (ChunkDB (symbolTable, termTable, conceptChunkTable, _unitTable, _dataDefnTable,
  _insmodelTable, _gendefTable, _theoryModelTable, _conceptinsTable,
  _citationTable, _labelledcontentTable, _traceTable, _refbyTable, _refTable,
  CDB), idMap, symbolMap, termMap, conceptMap, unitMap, addCdb)
import Language.Drasil (IdeaDict, Quantity, MayHaveUnit, Concept, IsUnit,
  ConceptChunk, ConceptInstance, Citation, Reference, LabelledContent, nw)
import Data.Drasil.Concepts.Documentation (doccon, doccon', srsDomains)
import Data.Drasil.Software.Products (prodtcon)
import Data.Drasil.Concepts.Education (educon)
import Data.Drasil.Concepts.Computation (compcon, algorithm)
import Data.Drasil.Concepts.Software (errMsg, program)
import Data.Drasil.Concepts.Math (mathcon)

import qualified Data.Map as Map (empty)
import Data.Drasil.SI_Units (siUnits)
import Theory.Drasil (DataDefinition, InstanceModel, TheoryModel, GenDefn)

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
  -- CIs
  --  * doccon' - A list of CommonIdeas that are added for the same purpose as `doccon`.
  map nw doccon'

basisConceptChunks :: [ConceptChunk]
basisConceptChunks =
  srsDomains ++ [algorithm, errMsg, program] ++ mathcon

-- | The basis chunk database, which contains the basic idea dicts, concept chunks,
--  and units that are used in all of the case studies. This database is then added
-- to all of the new chunk databases created using the cdb constructor.
basisCDB :: ChunkDB
basisCDB =
  CDB {
    -- CHUNKS
    symbolTable           = Map.empty,
    termTable             = termMap basisIdeaDicts,
    conceptChunkTable     = conceptMap basisConceptChunks,
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

  -- | Smart constructor for chunk databases. Takes in the following:
--
--     * ['Quantity'] (for 'SymbolMap'), 
--     * 'NamedIdea's (for 'TermMap'),
--     * 'Concept's (for 'ConceptMap'),
--     * Units (something that 'IsUnit' for 'UnitMap'),
--     * 'DataDefinition's (for 'DatadefnMap'),
--     * 'InstanceModel's (for 'InsModelMap'),
--     * 'GenDefn's (for 'GendefMap'),
--     * 'TheoryModel's (for 'TheoryModelMap'),
--     * 'ConceptInstance's (for 'ConceptInstanceMap'),
--     * 'LabelledContent's (for 'LabelledContentMap').
-- Creates a ChunkDB with basic data already included. Should be used over
-- cdb' in Database.Drasil, which does not include the basic data.
cdb :: (Quantity q, MayHaveUnit q, Concept c, IsUnit u) =>
    [q] -> [IdeaDict] -> [c] -> [u] -> [DataDefinition] -> [InstanceModel] ->
    [GenDefn] -> [TheoryModel] -> [ConceptInstance] ->
    [LabelledContent] -> [Reference] -> [Citation] -> ChunkDB
cdb s t c u d ins gd tm ci lc r cits =
  CDB {
    -- CHUNKS
    symbolTable = symbolMap s,
    termTable = termMap t,
    conceptChunkTable = conceptMap c,
    _unitTable = unitMap u,
    _dataDefnTable = idMap "DataDefnMap" d,
    _insmodelTable = idMap "InsModelMap" ins,
    _gendefTable = idMap "GenDefnmap" gd,
    _theoryModelTable = idMap "TheoryModelMap" tm,
    _conceptinsTable = idMap "ConcInsMap" ci,
    _citationTable = idMap "CiteMap" cits,
    -- NOT CHUNKS
    _labelledcontentTable = idMap "LLCMap" lc,
    _traceTable = Map.empty,
    _refbyTable = Map.empty,
    _refTable = idMap "RefMap" r
  } `addCdb` basisCDB