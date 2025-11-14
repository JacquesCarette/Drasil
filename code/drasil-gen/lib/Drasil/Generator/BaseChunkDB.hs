module Drasil.Generator.BaseChunkDB (
  -- * Base ChunkDB for all case studies
  cdb
) where

import Database.Drasil (empty, insertAll, ChunkDB, insertAllOutOfOrder12)
import Language.Drasil (IdeaDict, nw, Citation, ConceptChunk, ConceptInstance,
  DefinedQuantityDict, UnitDefn, LabelledContent, Reference)
import Data.Drasil.Concepts.Documentation (doccon, doccon', srsDomains)
import Data.Drasil.Software.Products (prodtcon)
import Data.Drasil.Concepts.Education (educon)
import Data.Drasil.Concepts.Computation (compcon, algorithm)
import Data.Drasil.Concepts.Software (errMsg, program)
import Data.Drasil.Concepts.Math (mathcon)

import Data.Drasil.SI_Units (siUnits)
import Theory.Drasil (DataDefinition, InstanceModel, TheoryModel, GenDefn)
import Language.Drasil.Code (codeDQDs)

basisSymbols :: [DefinedQuantityDict]
basisSymbols =
  -- | DefinedQuantityDicts
  --  * codeDQDs - A list of DefinedQuantityDicts that are used for general
  --               code generation in all case studies
  codeDQDs

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
  -- | ConceptChunks
  --  * algorithm - A concept chunk that describes algorithms. This is included in the
  --                basis as algorithms are a commonly used concept in the case studies.
  --                but did not fit into the other lists
  --  * errMsg - Concept chunk defining an error message. Error messages are common in
  --             any software, so this is included in the basis.
  --  * program - A concept chunk defining a computer program. This is included
  --              in the basis as all of the case studies are concerned with software.
  --  * srsDomains - SRS related concepts. These are included in the basis as every
  --                 case study should have a generated SRS, and these concepts would
  --                 be needed for that.
  --  * mathcon - Math concepts. Math is widespread throughout all of the case studies
  --              and scientific computing software in general, so it is included
  --              in the basis.
  [algorithm, errMsg, program] ++ srsDomains ++ mathcon

-- | The basis chunk database, which contains the basic idea dicts, concept chunks,
--  and units that are used in all of the case studies. This database is then added
-- to all of the new chunk databases created using the cdb constructor.
basisCDB :: ChunkDB
basisCDB =
    insertAll siUnits
  $ insertAll basisConceptChunks
  $ insertAll basisSymbols
  $ insertAll basisIdeaDicts
    empty

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
cdb :: [DefinedQuantityDict] -> [IdeaDict] -> [ConceptChunk] -> [UnitDefn] ->
    [DataDefinition] -> [InstanceModel] -> [GenDefn] -> [TheoryModel] ->
    [ConceptInstance] -> [LabelledContent] -> [Reference] -> [Citation] -> ChunkDB
cdb s t c u d ins gd tm ci lc r cits =
  insertAllOutOfOrder12 s t c u d ins gd tm ci cits lc r basisCDB
