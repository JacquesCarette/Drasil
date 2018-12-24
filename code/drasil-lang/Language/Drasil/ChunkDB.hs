{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.ChunkDB 
  ( ChunkDB, cdb
  , HasSymbolTable(..), symbolMap, symbLookup
  , HasTermTable(..), termLookup
  , HasDefinitionTable(..), conceptMap, traceMap, defLookup
  , unitLookup , HasUnitTable(..), unitMap, collectUnits, TraceMap
  , traceLookup, HasTraceTable(..), generateRefbyMap, RefbyMap, LabelledContentMap
  , refbyLookup, HasRefbyTable(..), DatadefnMap, InsModelMap, SectionMap
  , GendefMap, TheoryModelMap, AssumptionMap, ConceptInstanceMap, datadefnLookup
  , insmodelLookup, gendefLookup, theoryModelLookup, assumptionLookup, conceptinsLookup
  , sectionLookup, labelledconLookup
  , HasDataDefnTable(..), HasInsModelTable(..), HasGendefTable(..), HasTheoryModelTable(..)
  , HasAssumpTable(..), HasConceptInstance(..), HasSectionTable(..), HasLabelledContent(..)
  ) where

import Control.Lens ((^.), Lens', makeLenses)
import Data.Maybe (maybeToList)
import Language.Drasil.UID (UID)
import Language.Drasil.Classes.Core (HasUID(uid))
import Language.Drasil.Classes (Concept, ConceptDomain, Idea, IsUnit, Quantity)
import Language.Drasil.Chunk.AssumpChunk (AssumpChunk)
import Language.Drasil.Chunk.DataDefinition (DataDefinition)
import Language.Drasil.Document.Core (LabelledContent)
import Language.Drasil.Chunk.NamedIdea (IdeaDict, nw)
import Language.Drasil.Chunk.Quantity (QuantityDict, qw)
import Language.Drasil.Chunk.Concept (ConceptChunk, ConceptInstance, cw)
import Language.Drasil.Chunk.InstanceModel (InstanceModel)
import Language.Drasil.Chunk.GenDefn (GenDefn)
import Language.Drasil.Chunk.Theory (TheoryModel)
import Language.Drasil.Document (Section)
import Language.Drasil.Development.Unit(UnitDefn, MayHaveUnit(getUnit), unitWrapper, IsUnit(getUnits))
import qualified Data.Map as Map

-- The misnomers below are not actually a bad thing, we want to ensure data can't
-- be added to a map if it's not coming from a chunk, and there's no point confusing
-- what the map is for. One is for symbols + their units, and the others are for
-- what they state.
type UMap a = Map.Map UID a

-- | A bit of a misnomer as it's really a map of all quantities, for retrieving
-- symbols and their units.
type SymbolMap  = UMap QuantityDict

-- | A map of all concepts, normally used for retrieving definitions.
type ConceptMap = UMap ConceptChunk

-- | A map of all the units used. Should be restricted to base units/synonyms.
type UnitMap = UMap UnitDefn

-- | Again a bit of a misnomer as it's really a map of all NamedIdeas.
-- Until these are built through automated means, there will
-- likely be some 'manual' duplication of terms as this map will contain all
-- quantities, concepts, etc.
type TermMap = UMap IdeaDict

-- Reference map
type TraceMap = UMap [UID]

-- Refby map
type RefbyMap = Map.Map UID [UID]

-- DataDefinition map
type DatadefnMap = UMap DataDefinition

-- InstanceModel map
type InsModelMap = UMap InstanceModel

-- GenDef map
type GendefMap = UMap GenDefn

-- TheoryModel map
type TheoryModelMap = UMap TheoryModel

-- Assumption map
type AssumptionMap = UMap AssumpChunk

-- ConceptInstance map
type ConceptInstanceMap = UMap ConceptInstance

-- Section map
type SectionMap = UMap Section

-- LabelledContent map
type LabelledContentMap = UMap LabelledContent

cdbMap :: HasUID a => (a -> b) -> [a] -> Map.Map UID b
cdbMap fn = Map.fromList . map (\x -> (x ^. uid, fn x))

-- | Smart constructor for a 'SymbolMap'
symbolMap :: (Quantity c, MayHaveUnit c) => [c] -> SymbolMap
symbolMap = cdbMap qw

-- | Smart constructor for a 'TermMap'
termMap :: (Idea c) => [c] -> TermMap
termMap = cdbMap nw

-- | Smart constructor for a 'ConceptMap'
conceptMap :: (Concept c) => [c] -> ConceptMap
conceptMap = cdbMap cw

-- | Smart constructor for a 'UnitMap'
unitMap :: (IsUnit u) => [u] -> UnitMap
unitMap = cdbMap unitWrapper

idMap :: HasUID a => [a] -> Map.Map UID a
idMap = cdbMap id

traceMap :: HasUID l => (l -> [UID]) -> [l] -> TraceMap
traceMap f = cdbMap f

-- | Looks up an uid in the symbol table. If nothing is found, an error is thrown
symbLookup :: UID -> SymbolMap -> QuantityDict
symbLookup c m = getS $ Map.lookup c m
  where getS = maybe (error $ "Symbol: " ++ c ++ " not found in SymbolMap") id

--- SYMBOL TABLE ---
class HasSymbolTable s where
  symbolTable :: Lens' s SymbolMap

--- TERM TABLE ---
class HasTermTable s where
  termTable :: Lens' s TermMap
  
--- DEFINITION TABLE ---
class HasDefinitionTable s where
  defTable :: Lens' s ConceptMap

--- UNIT TABLE ---
class HasUnitTable s where
  unitTable :: Lens' s UnitMap

-- TRACE TABLE --
class HasTraceTable s where
  traceTable :: Lens' s TraceMap

-- Refby TABLE --
class HasRefbyTable s where
  refbyTable :: Lens' s RefbyMap

--- DataDefn TABLE ---
class HasDataDefnTable s where
  dataDefnTable :: Lens' s DatadefnMap

-- Instance Model TABLE --
class HasInsModelTable s where
  insmodelTable :: Lens' s InsModelMap

-- General Definition TABLE --
class HasGendefTable s where
  gendefTable :: Lens' s GendefMap

-- Theory Model TABLE --
class HasTheoryModelTable s where
  theoryModelTable :: Lens' s TheoryModelMap

-- Assumption TABLE --
class HasAssumpTable s where
  assumpTable :: Lens' s AssumptionMap

class HasConceptInstance s where
  conceptinsTable :: Lens' s ConceptInstanceMap

class HasSectionTable s where
  sectionTable :: Lens' s SectionMap

class HasLabelledContent s where
  labelledcontent :: Lens' s LabelledContentMap

-- | Gets a unit if it exists, or Nothing.        
getUnitLup :: HasSymbolTable s => HasUID c => s -> c -> Maybe UnitDefn
getUnitLup m c = getUnit $ symbLookup (c ^. uid) (m ^. symbolTable)

-- | Looks up an uid in the term table. If nothing is found, an error is thrown
termLookup :: UID -> TermMap -> IdeaDict
termLookup c m = getT $ Map.lookup c m
  where getT = maybe (error $ "Term: " ++ c ++ " not found in TermMap") id

-- | Looks up an uid in the term table. If nothing is found, an error is thrown
unitLookup :: UID -> UnitMap -> UnitDefn
unitLookup c m = getT $ Map.lookup c m
  where getT = maybe (error $ "Unit: " ++ c ++ " not found in UnitMap") id

-- | Looks up a uid in the definition table. If nothing is found, an error is thrown.
defLookup :: UID -> ConceptMap -> ConceptChunk
defLookup u m = getC $ Map.lookup u m
  where getC = maybe (error $ "Concept: " ++ u ++ " not found in ConceptMap") id


-- | Looks up a uid in the datadefinition table. If nothing is found, an error is thrown.
datadefnLookup :: UID -> DatadefnMap -> DataDefinition
datadefnLookup u m = getC $ Map.lookup u m
  where getC = maybe (error $ "DataDefinition: " ++ u ++ " not found in datadefnMap") id


-- | Looks up a uid in the definition table. If nothing is found, an error is thrown.
insmodelLookup :: UID -> InsModelMap -> InstanceModel
insmodelLookup u m = getC $ Map.lookup u m
  where getC = maybe (error $ "InstanceModel: " ++ u ++ " not found in insModelMap") id


-- | Looks up a uid in the definition table. If nothing is found, an error is thrown.
gendefLookup :: UID -> GendefMap -> GenDefn
gendefLookup u m = getC $ Map.lookup u m
  where getC = maybe (error $ "GeneralDefinition: " ++ u ++ " not found in GendefMap") id


-- | Looks up a uid in the definition table. If nothing is found, an error is thrown.
theoryModelLookup :: UID -> TheoryModelMap -> TheoryModel
theoryModelLookup u m = getC $ Map.lookup u m
  where getC = maybe (error $ "TheoryModel: " ++ u ++ " not found in TheoryModelMap") id

-- | Looks up a uid in the definition table. If nothing is found, an error is thrown.
assumptionLookup :: UID -> AssumptionMap -> AssumpChunk
assumptionLookup u m = getC $ Map.lookup u m
  where getC = maybe (error $ "Assumption: " ++ u ++ " not found in AssumptionMap") id

-- | Looks up a uid in the definition table. If nothing is found, an error is thrown.
conceptinsLookup :: UID -> ConceptInstanceMap -> ConceptInstance
conceptinsLookup u m = getC $ Map.lookup u m
  where getC = maybe (error $ "ConceptInstance: " ++ u ++ " not found in conceptInstanceMap") id

-- | Looks up a uid in the definition table. If nothing is found, an error is thrown.
sectionLookup :: UID -> SectionMap -> Section
sectionLookup u m = getC $ Map.lookup u m
  where getC = maybe (error $ "Section: " ++ u ++ " not found in sectionMap") id

-- | Looks up a uid in the definition table. If nothing is found, an error is thrown.
labelledconLookup :: UID -> LabelledContentMap -> LabelledContent
labelledconLookup u m = getC $ Map.lookup u m
  where getC = maybe (error $ "LabelledContent: " ++ u ++ " not found in LabelledContentMap") id

-- | Our chunk databases. Should contain all the maps we will need.
data ChunkDB = CDB { _csymbs :: SymbolMap
                   , _cterms :: TermMap 
                   , _cdefs  :: ConceptMap
                   , _cunitDB :: UnitMap
                   , _ctrace :: TraceMap
                   , _crefby :: RefbyMap
                   , _cdata  :: DatadefnMap
                   , _cins   :: InsModelMap
                   , _cgen   :: GendefMap
                   , _ctheory :: TheoryModelMap
                   , _cassump :: AssumptionMap
                   , _cconceptins :: ConceptInstanceMap
                   , _csec :: SectionMap
                   , _clabelled :: LabelledContentMap
                   } --TODO: Expand and add more databases
makeLenses ''ChunkDB

-- | Smart constructor for chunk databases. Takes a list of Quantities 
-- (for SymbolTable), NamedIdeas (for TermTable), Concepts (for DefinitionTable),
-- and Units (for UnitTable)
cdb :: (Quantity q, MayHaveUnit q, Idea t, Concept c, IsUnit u,
        ConceptDomain u) => [q] -> [t] -> [c] -> [u] -> TraceMap -> RefbyMap ->
        [DataDefinition] -> [InstanceModel] -> [GenDefn] ->  [TheoryModel] -> [AssumpChunk] ->
        [ConceptInstance] -> [Section] -> [LabelledContent] -> ChunkDB
cdb s t c u tc rfm dd ins gd tm a ci sec lc = CDB (symbolMap s) (termMap t) (conceptMap c) (unitMap u)
 tc rfm (idMap dd) (idMap ins) (idMap gd) (idMap tm) (idMap a) (idMap ci) (idMap sec) (idMap lc)

----------------------
instance HasSymbolTable      ChunkDB where symbolTable       = csymbs
instance HasTermTable        ChunkDB where termTable         = cterms
instance HasDefinitionTable  ChunkDB where defTable          = cdefs
instance HasUnitTable        ChunkDB where unitTable         = cunitDB
instance HasTraceTable       ChunkDB where traceTable        = ctrace
instance HasRefbyTable       ChunkDB where refbyTable        = crefby
instance HasDataDefnTable    ChunkDB where dataDefnTable     = cdata
instance HasInsModelTable    ChunkDB where insmodelTable     = cins
instance HasGendefTable      ChunkDB where gendefTable       = cgen
instance HasTheoryModelTable ChunkDB where theoryModelTable  = ctheory
instance HasAssumpTable      ChunkDB where assumpTable       = cassump
instance HasConceptInstance  ChunkDB where conceptinsTable   = cconceptins
instance HasSectionTable     ChunkDB where sectionTable      = csec
instance HasLabelledContent  ChunkDB where labelledcontent  = clabelled

collectUnits :: (HasSymbolTable s, HasUnitTable s) => (Quantity c, MayHaveUnit c) => s -> [c] -> [UnitDefn]
collectUnits m symb = map unitWrapper $ map (\x -> unitLookup x $ m ^. unitTable)
 $ concatMap getUnits $ concatMap maybeToList $ map (getUnitLup m) symb

traceLookup :: UID -> TraceMap -> [UID]
traceLookup c m = getT $ Map.lookup c m
  where getT = maybe [] id
 
invert :: (Ord v) => Map.Map k [v] -> Map.Map v [k]
invert m = Map.fromListWith (++) pairs
    where pairs = [(v, [k]) | (k, vs) <- Map.toList m, v <- vs]
 
generateRefbyMap :: TraceMap  -> RefbyMap
generateRefbyMap tm = invert tm

refbyLookup :: UID -> RefbyMap -> [UID]
refbyLookup c m = getT $ Map.lookup c m
  where getT = maybe [] id
