{-# LANGUAGE TemplateHaskell #-}
module Database.Drasil.ChunkDB (ChunkDB, RefbyMap, TraceMap, UMap,
  asOrderedList, cdb, collectUnits, conceptMap, conceptinsLookup,
  conceptinsTable, dataDefnTable, datadefnLookup, defResolve, gendefLookup,
  gendefTable, generateRefbyMap, insmodelLookup, insmodelTable,
  labelledconLookup, labelledcontentTable, refbyLookup, refbyTable,
  sectionLookup, sectionTable, symbResolve, termResolve, termTable,
  theoryModelLookup, theoryModelTable, traceLookup, traceMap, traceTable) where

import Language.Drasil
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)

import Control.Lens ((^.), makeLenses)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
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
type TraceMap = Map.Map UID [UID]
type RefbyMap = Map.Map UID [UID]
type DatadefnMap = UMap DataDefinition
type InsModelMap = UMap InstanceModel
type GendefMap = UMap GenDefn
type TheoryModelMap = UMap TheoryModel
type ConceptInstanceMap = UMap ConceptInstance
type SectionMap = UMap Section
type LabelledContentMap = UMap LabelledContent

cdbMap :: HasUID a => (a -> b) -> [a] -> Map.Map UID (b, Int)
cdbMap fn = Map.fromList . map (\(x,y) -> (x ^. uid, (fn x, y))) . flip zip [1..]

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

traceMap :: [(UID, [UID])] -> TraceMap
traceMap = Map.fromList

-- | Gets a unit if it exists, or Nothing.        
getUnitLup :: HasUID c => ChunkDB -> c -> Maybe UnitDefn
getUnitLup m c = getUnit $ symbResolve m (c ^. uid)

-- | Looks up a UID in a UMap table. If nothing is found an error is thrown
uMapLookup :: String -> String -> UID -> UMap a -> a
uMapLookup tys ms u t = getFM $ Map.lookup u t
  where getFM = maybe (error $ tys ++ ": " ++ u ++ " not found in " ++ ms) fst

-- | Looks up a UID in the symbol table from the ChunkDB. If nothing is found, an error is thrown.
symbResolve :: ChunkDB -> UID -> QuantityDict
symbResolve m x = uMapLookup "Symbol" "SymbolMap" x $ symbolTable m

-- | Looks up a UID in the term table from the ChunkDB. If nothing is found, an error is thrown.
termResolve :: ChunkDB -> UID -> IdeaDict
termResolve m x = uMapLookup "Term" "TermMap" x $ termTable m

-- | Looks up a UID in the unit table. If nothing is found, an error is thrown.
unitLookup :: UID -> UnitMap -> UnitDefn
unitLookup = uMapLookup "Unit" "UnitMap"

-- | Looks up a UID in the definition table from the ChunkDB. If nothing is found, an error is thrown.
defResolve :: ChunkDB -> UID -> ConceptChunk
defResolve m x = uMapLookup "Concept" "ConceptMap" x $ defTable m

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

-- | Looks up a uid in the concept instance table. If nothing is found, an error is thrown.
conceptinsLookup :: UID -> ConceptInstanceMap -> ConceptInstance
conceptinsLookup = uMapLookup "ConceptInstance" "ConceptInstanceMap"

-- | Looks up a uid in the section table. If nothing is found, an error is thrown.
sectionLookup :: UID -> SectionMap -> Section
sectionLookup = uMapLookup "Section" "SectionMap"

-- | Looks up a uid in the labelled content table. If nothing is found, an error is thrown.
labelledconLookup :: UID -> LabelledContentMap -> LabelledContent
labelledconLookup = uMapLookup "LabelledContent" "LabelledContentMap"

asOrderedList :: UMap a -> [a]
asOrderedList = map fst . sortOn snd . map snd . Map.toList

-- | Our chunk databases. Should contain all the maps we will need.
data ChunkDB = CDB { symbolTable :: SymbolMap
                   , termTable :: TermMap 
                   , defTable  :: ConceptMap
                   , _unitTable :: UnitMap
                   , _traceTable :: TraceMap
                   , _refbyTable :: RefbyMap
                   , _dataDefnTable  :: DatadefnMap
                   , _insmodelTable   :: InsModelMap
                   , _gendefTable   :: GendefMap
                   , _theoryModelTable :: TheoryModelMap
                   , _conceptinsTable :: ConceptInstanceMap
                   , _sectionTable :: SectionMap
                   , _labelledcontentTable :: LabelledContentMap
                   } --TODO: Expand and add more databases
makeLenses ''ChunkDB

-- | Smart constructor for chunk databases. Takes a list of Quantities 
-- (for SymbolTable), NamedIdeas (for TermTable), Concepts (for DefinitionTable),
-- and Units (for UnitTable)
cdb :: (Quantity q, MayHaveUnit q, Idea t, Concept c, IsUnit u) =>
    [q] -> [t] -> [c] -> [u] -> [DataDefinition] -> [InstanceModel] ->
    [GenDefn] -> [TheoryModel] -> [ConceptInstance] -> [Section] ->
    [LabelledContent] -> ChunkDB
cdb s t c u d ins gd tm ci sect lc = CDB (symbolMap s) (termMap t) (conceptMap c)
  (unitMap u) Map.empty Map.empty (idMap d) (idMap ins) (idMap gd) (idMap tm)
  (idMap ci) (idMap sect) (idMap lc)

collectUnits :: Quantity c => ChunkDB -> [c] -> [UnitDefn]
collectUnits m = map (unitWrapper . flip unitLookup (m ^. unitTable))
 . concatMap getUnits . mapMaybe (getUnitLup m)

traceLookup :: UID -> TraceMap -> [UID]
traceLookup c = fromMaybe [] . Map.lookup c
 
invert :: (Ord v) => Map.Map k [v] -> Map.Map v [k]
invert m = Map.fromListWith (++) pairs
    where pairs = [(v, [k]) | (k, vs) <- Map.toList m, v <- vs]
 
generateRefbyMap :: TraceMap -> RefbyMap
generateRefbyMap = invert

refbyLookup :: UID -> RefbyMap -> [UID]
refbyLookup c = fromMaybe [] . Map.lookup c
