{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.ChunkDB 
  ( ChunkDB, cdb
  , symbolTable, symbLookup
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
import Language.Drasil.Classes (Concept, Idea, IsUnit, Quantity)
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
import Language.Drasil.Chunk.UnitDefn (UnitDefn, MayHaveUnit(getUnit), unitWrapper
  , IsUnit(getUnits))
import qualified Data.Map as Map

-- The misnomers below are not actually a bad thing, we want to ensure data can't
-- be added to a map if it's not coming from a chunk, and there's no point confusing
-- what the map is for. One is for symbols + their units, and the others are for
-- what they state.
type UMap a = Map.Map UID (a, Int)

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

cdbMap :: HasUID a => (a -> b) -> [a] -> Map.Map UID (b, Int)
cdbMap fn = Map.fromList . map (\(x,y) -> (x ^. uid, (fn x, y))) . (flip zip) [1..]

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

idMap :: HasUID a => [a] -> Map.Map UID (a, Int)
idMap = cdbMap id

traceMap :: HasUID l => (l -> [UID]) -> [l] -> TraceMap
traceMap f = cdbMap f

-- | Looks up an uid in the symbol table. If nothing is found, an error is thrown
symbLookup :: UID -> SymbolMap -> QuantityDict
symbLookup c m = getS $ Map.lookup c m
  where getS = maybe (error $ "Symbol: " ++ c ++ " not found in SymbolMap") fst

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
getUnitLup :: HasUID c => ChunkDB -> c -> Maybe UnitDefn
getUnitLup m c = getUnit $ symbLookup (c ^. uid) (symbolTable m)

-- | Looks up a UID in a UMap table. If nothing is found an error is thrown
uMapLookup :: String -> String -> UID -> UMap a -> a
uMapLookup tys ms u t = getFM $ Map.lookup u t
  where getFM = maybe (error $ tys ++ ": " ++ u ++ " not found in " ++ ms) fst

-- | Looks up an uid in the term table. If nothing is found, an error is thrown
termLookup :: UID -> TermMap -> IdeaDict
termLookup = uMapLookup "Term" "TermMap"

-- | Looks up an uid in the unit table. If nothing is found, an error is thrown
unitLookup :: UID -> UnitMap -> UnitDefn
unitLookup = uMapLookup "Unit" "UnitMap"

-- | Looks up a uid in the definition table. If nothing is found, an error is thrown.
defLookup :: UID -> ConceptMap -> ConceptChunk
defLookup = uMapLookup "Concept" "ConceptMap"

-- | Looks up a uid in the datadefinition table. If nothing is found, an error is thrown.
datadefnLookup :: UID -> DatadefnMap -> DataDefinition
datadefnLookup = uMapLookup "DataDefinition" "DatadefnMap"

-- | Looks up a uid in the instance model table. If nothing is found, an error is thrown.
insmodelLookup :: UID -> InsModelMap -> InstanceModel
insmodelLookup = uMapLookup "InstanceModel" "InsModelMap"

-- | Looks up a uid in the general definition table. If nothing is found, an error is thrown.
gendefLookup :: UID -> GendefMap -> GenDefn
gendefLookup = uMapLookup "GenDefn" "GenDefnMap" 

-- | Looks up a uid in the theory model table. If nothing is found, an error is thrown.
theoryModelLookup :: UID -> TheoryModelMap -> TheoryModel
theoryModelLookup = uMapLookup "TheoryModel" "TheoryModelMap"

-- | Looks up a uid in the assumption table. If nothing is found, an error is thrown.
assumptionLookup :: UID -> AssumptionMap -> AssumpChunk
assumptionLookup = uMapLookup "Assumption" "AssumptionMap"

-- | Looks up a uid in the concept instance table. If nothing is found, an error is thrown.
conceptinsLookup :: UID -> ConceptInstanceMap -> ConceptInstance
conceptinsLookup = uMapLookup "ConceptInstance" "ConceptInstanceMap"

-- | Looks up a uid in the section table. If nothing is found, an error is thrown.
sectionLookup :: UID -> SectionMap -> Section
sectionLookup = uMapLookup "Section" "SectionMap"

-- | Looks up a uid in the labelled content table. If nothing is found, an error is thrown.
labelledconLookup :: UID -> LabelledContentMap -> LabelledContent
labelledconLookup = uMapLookup "LabelledContent" "LabelledContentMap"

-- | Our chunk databases. Should contain all the maps we will need.
data ChunkDB = CDB { symbolTable :: SymbolMap
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
cdb :: (Quantity q, MayHaveUnit q, Idea t, Concept c, IsUnit u) =>
    [q] -> [t] -> [c] -> [u] -> TraceMap -> RefbyMap ->
    [DataDefinition] -> [InstanceModel] -> [GenDefn] ->  [TheoryModel] -> [AssumpChunk] ->
    [ConceptInstance] -> [Section] -> [LabelledContent] -> ChunkDB
cdb s t c u tc rfm dd ins gd tm a ci sec lc = CDB (symbolMap s) (termMap t)
  (conceptMap c) (unitMap u) tc rfm (idMap dd) (idMap ins) (idMap gd) (idMap tm)
  (idMap a) (idMap ci) (idMap sec) (idMap lc)

----------------------
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

collectUnits :: Quantity c => ChunkDB -> [c] -> [UnitDefn]
collectUnits m symb = map unitWrapper $ map (\x -> unitLookup x $ m ^. unitTable)
 $ concatMap getUnits $ concatMap maybeToList $ map (getUnitLup m) symb

traceLookup :: UID -> TraceMap -> [UID]
traceLookup c m = getT $ Map.lookup c m
  where getT = maybe [] fst
 
invert :: (Ord v) => Map.Map k [v] -> Map.Map v [k]
invert m = Map.fromListWith (++) pairs
    where pairs = [(v, [k]) | (k, vs) <- Map.toList m, v <- vs]
 
generateRefbyMap :: TraceMap  -> RefbyMap
generateRefbyMap tm = invert $ Map.map (\(x,_) -> x) tm

refbyLookup :: UID -> RefbyMap -> [UID]
refbyLookup c m = getT $ Map.lookup c m
  where getT = maybe [] id
