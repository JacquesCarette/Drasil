module MetaDatabase.Drasil.BasisCDB (
    cdb, basisCDB
) where

import Database.Drasil (ChunkDB(..), symbolMap, termMap, conceptMap, unitMap, idMap, addCdb)

import Data.Drasil.Concepts.Documentation (doccon, srsDomains, doccon')
import Data.Drasil.Software.Products (prodtcon)
import Data.Drasil.Concepts.Computation (algorithm, compcon)
import Data.Drasil.Concepts.Software (errMsg, program)
import Data.Drasil.Concepts.Math (mathcon)
import Data.Drasil.Concepts.Education (educon)

import qualified Data.Map as Map (empty)

import Language.Drasil (Quantity, MayHaveUnit(..),
  IdeaDict, Concept, IsUnit(..), Reference, ConceptInstance, LabelledContent,
  Citation, nw)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)

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
    _unitTable            = Map.empty,
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