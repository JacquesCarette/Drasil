{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.ChunkDB 
  ( ChunkDB, cdb
  , symbolTable, symbLookup
  , termLookup, termTable
  , conceptMap, traceMap, defLookup, defTable
  , unitLookup , unitTable, collectUnits
  , traceLookup, traceTable, TraceMap, generateRefbyMap, RefbyMap
  , refbyLookup, refbyTable
  , datadefnLookup
  , insmodelLookup, gendefLookup, theoryModelLookup, assumptionLookup, conceptinsLookup
  , sectionLookup, labelledconLookup
  , dataDefnTable, insmodelTable, gendefTable, theoryModelTable
  , assumpTable, conceptinsTable, sectionTable, labelledcontentTable, asOrderedList
  ) where

import Control.Lens ((^.), makeLenses)
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
import Data.List (sortOn)

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
type TraceMap = UMap [UID]
type RefbyMap = Map.Map UID [UID]
type DatadefnMap = UMap DataDefinition
type InsModelMap = UMap InstanceModel
type GendefMap = UMap GenDefn
type TheoryModelMap = UMap TheoryModel
type AssumptionMap = UMap AssumpChunk
type ConceptInstanceMap = UMap ConceptInstance
type SectionMap = UMap Section
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
                   , _assumpTable :: AssumptionMap
                   , _conceptinsTable :: ConceptInstanceMap
                   , _sectionTable :: SectionMap
                   , _labelledcontentTable :: LabelledContentMap
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

collectUnits :: Quantity c => ChunkDB -> [c] -> [UnitDefn]
collectUnits m symb = map unitWrapper $ map (\x -> unitLookup x $ m ^. unitTable)
 $ concatMap getUnits $ concatMap maybeToList $ map (getUnitLup m) symb

traceLookup :: UID -> TraceMap -> [UID]
traceLookup c m = maybe [] fst $ Map.lookup c m
 
invert :: (Ord v) => Map.Map k [v] -> Map.Map v [k]
invert m = Map.fromListWith (++) pairs
    where pairs = [(v, [k]) | (k, vs) <- Map.toList m, v <- vs]
 
generateRefbyMap :: TraceMap  -> RefbyMap
generateRefbyMap = invert . Map.map fst

refbyLookup :: UID -> RefbyMap -> [UID]
refbyLookup c m = maybe [] id $ Map.lookup c m
