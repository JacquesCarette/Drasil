module Drasil.Generator.CommonKnowledge (
  -- * Common Background Knowledge for Drasil's Science-focused Case Studies
  withCommonKnowledge
) where

import Drasil.Database (empty, insertAll, ChunkDB, insertAllOutOfOrder12)
import Language.Drasil (IdeaDict, nw, Citation, ConceptChunk, ConceptInstance,
  DefinedQuantityDict, UnitDefn, LabelledContent, Reference)
import Data.Drasil.Citations (cartesianWiki, lineSource, pointSource)
import Data.Drasil.Concepts.Documentation (doccon, doccon', srsDomains)
import Data.Drasil.Software.Products (prodtcon)
import Data.Drasil.Concepts.Education (educon)
import Data.Drasil.Concepts.Computation (compcon, algorithm)
import Data.Drasil.Concepts.Software (errMsg, program)
import Data.Drasil.Concepts.Math (mathcon)
import Data.Drasil.SI_Units (siUnits)
import qualified Drasil.DocLang.SRS as SRS
import Theory.Drasil (DataDefinition, InstanceModel, TheoryModel, GenDefn)
import Language.Drasil.Code (codeDQDs)

-- | Create a `ChunkDB` containing background knowledge common to all of
-- Drasil's existing case studies. This means knowledge related to the
-- SmithEtAl-esque SRS, mathematics, physics, general science, basic software,
-- and general documentation.
withCommonKnowledge :: [Reference] -> [DefinedQuantityDict] -> [IdeaDict] ->
    [ConceptChunk] -> [UnitDefn] -> [DataDefinition] -> [InstanceModel] ->
    [GenDefn] -> [TheoryModel] -> [ConceptInstance] -> [Citation] ->
    [LabelledContent] -> ChunkDB
withCommonKnowledge = insertAllOutOfOrder12 basisCDB

-- | The 'basis' chunk database to all of Drasil's case studies, containing
-- common background knowledge, including that related to the SRS, mathematics,
-- physics, general science, basic software, and general documentation.
basisCDB :: ChunkDB
basisCDB =
    insertAll basisReferences
  $ insertAll siUnits
  $ insertAll basisConceptChunks
  $ insertAll basisSymbols
  $ insertAll basisIdeaDicts
  $ insertAll basisCitations
    empty

basisReferences :: [Reference]
basisReferences = SRS.sectionReferences

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

basisCitations :: [Citation]
basisCitations = [cartesianWiki, lineSource, pointSource]
