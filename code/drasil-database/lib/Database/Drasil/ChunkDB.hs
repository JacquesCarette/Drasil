{-# LANGUAGE TemplateHaskell #-}
-- | Defines types and functions to create a chunk database within Drasil.

-- Changes to ChunkDB should be reflected in the 'Creating Your Project 
-- in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil
module Database.Drasil.ChunkDB (
  -- * Types
  -- ** 'ChunkDB'
  -- | Main database type
  ChunkDB(CDB, symbolTable, termTable, defTable),
  -- ** Maps
  -- | Exported for external use.
  RefbyMap, TraceMap, UMap,
  -- * Functions
  -- ** Constructors
  cdb, idMap, termMap, conceptMap, traceMap, generateRefbyMap, -- idMap, termMap for docLang
  -- ** Lookup Functions
  asOrderedList, collectUnits,
  termResolve, defResolve, symbResolve,
  traceLookup, refbyLookup,
  datadefnLookup, insmodelLookup, gendefLookup, theoryModelLookup,
  conceptinsLookup, refResolve,
  -- ** Lenses
  unitTable, traceTable, refbyTable,
  dataDefnTable, insmodelTable, gendefTable, theoryModelTable,
  conceptinsTable, labelledcontentTable, refTable
) where

import Language.Drasil
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)

import Control.Lens ((^.), makeLenses)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map as Map
import Utils.Drasil (invert)

-- | The misnomers below (for the following Map types) are not actually a bad thing. We want to ensure data can't
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

-- | Again a bit of a misnomer as it's really a map of all 'NamedIdea's.
-- Until these are built through automated means, there will
-- likely be some 'manual' duplication of terms as this map will contain all
-- quantities, concepts, etc.
type TermMap = UMap IdeaDict
-- | A traceability map, used to hold the relation between one 'UID' and a list of other 'UID's.
type TraceMap = Map.Map UID [UID]
-- | A reference map, used to hold a 'UID' and where it is referenced ('UID's).
type RefbyMap = Map.Map UID [UID]
-- | Data definitions map. Contains all data definitions ('DataDefinition').
type DatadefnMap = UMap DataDefinition
-- | Instance model map. Contains all instance models ('InstanceModel').
type InsModelMap = UMap InstanceModel
-- | General definitions map. Contains all general definitions ('GenDefn').
type GendefMap = UMap GenDefn
-- | Theory model map. Contains all theoretical models ('TheoryModel').
type TheoryModelMap = UMap TheoryModel
-- | Concept instance map. May hold similar information to a 'ConceptMap', but may also be referred to.
type ConceptInstanceMap = UMap ConceptInstance
-- | A map of all 'LabelledContent's.
type LabelledContentMap = UMap LabelledContent
-- | A map of all 'Reference's.
type ReferenceMap = UMap Reference

-- | General chunk database map constructor. Creates a 'UMap' from a function that converts something with 'UID's into another type and a list of something with 'UID's.
cdbMap :: HasUID a => (a -> b) -> [a] -> Map.Map UID (b, Int)
cdbMap fn = Map.fromList . map (\(x,y) -> (x ^. uid, (fn x, y))) . flip zip [1..]

-- | Smart constructor for a 'SymbolMap'.
symbolMap :: (Quantity c, MayHaveUnit c) => [c] -> SymbolMap
symbolMap = cdbMap qw

-- | Smart constructor for a 'TermMap'.
termMap :: (Idea c) => [c] -> TermMap
termMap = cdbMap nw

-- | Smart constructor for a 'ConceptMap'.
conceptMap :: (Concept c) => [c] -> ConceptMap
conceptMap = cdbMap cw

-- | Smart constructor for a 'UnitMap'.
unitMap :: (IsUnit u) => [u] -> UnitMap
unitMap = cdbMap unitWrapper

-- | General smart constructor for making a 'UMap' out of anything that has a 'UID'. 
idMap :: HasUID a => [a] -> Map.Map UID (a, Int)
idMap = cdbMap id

-- | Smart constructor for a 'TraceMap' given a traceability matrix.
traceMap :: [(UID, [UID])] -> TraceMap
traceMap = Map.fromList

-- | Gets a unit if it exists, or Nothing.        
getUnitLup :: HasUID c => ChunkDB -> c -> Maybe UnitDefn
getUnitLup m c = getUnit $ symbResolve m (c ^. uid)

-- | Looks up a 'UID' in a 'UMap' table. If nothing is found, an error is thrown.
uMapLookup :: String -> String -> UID -> UMap a -> a
uMapLookup tys ms u t = getFM $ Map.lookup u t
  where getFM = maybe (error $ tys ++ ": " ++ show u ++ " not found in " ++ ms) fst

-- | Looks up a 'UID' in the symbol table from the 'ChunkDB'. If nothing is found, an error is thrown.
symbResolve :: ChunkDB -> UID -> QuantityDict
symbResolve m x = uMapLookup "Symbol" "SymbolMap" x $ symbolTable m

-- | Looks up a 'UID' in the term table from the 'ChunkDB'. If nothing is found, an error is thrown.
termResolve :: ChunkDB -> UID -> IdeaDict
termResolve m x = uMapLookup "Term" "TermMap" x $ termTable m

-- | Looks up a 'UID' in the reference table from the 'ChunkDB'. If nothing is found, an error is thrown.
refResolve :: UID -> ReferenceMap -> Reference
refResolve = uMapLookup "Reference" "ReferenceMap"

-- | Looks up a 'UID' in the unit table. If nothing is found, an error is thrown.
unitLookup :: UID -> UnitMap -> UnitDefn
unitLookup = uMapLookup "Unit" "UnitMap"

-- | Looks up a 'UID' in the definition table from the 'ChunkDB'. If nothing is found, an error is thrown.
defResolve :: ChunkDB -> UID -> ConceptChunk
defResolve m x = uMapLookup "Concept" "ConceptMap" x $ defTable m

-- | Looks up a 'UID' in the datadefinition table. If nothing is found, an error is thrown.
datadefnLookup :: UID -> DatadefnMap -> DataDefinition
datadefnLookup = uMapLookup "DataDefinition" "DatadefnMap"

-- | Looks up a 'UID' in the instance model table. If nothing is found, an error is thrown.
insmodelLookup :: UID -> InsModelMap -> InstanceModel
insmodelLookup = uMapLookup "InstanceModel" "InsModelMap"

-- | Looks up a 'UID' in the general definition table. If nothing is found, an error is thrown.
gendefLookup :: UID -> GendefMap -> GenDefn
gendefLookup = uMapLookup "GenDefn" "GenDefnMap" 

-- | Looks up a 'UID' in the theory model table. If nothing is found, an error is thrown.
theoryModelLookup :: UID -> TheoryModelMap -> TheoryModel
theoryModelLookup = uMapLookup "TheoryModel" "TheoryModelMap"

-- | Looks up a 'UID' in the concept instance table. If nothing is found, an error is thrown.
conceptinsLookup :: UID -> ConceptInstanceMap -> ConceptInstance
conceptinsLookup = uMapLookup "ConceptInstance" "ConceptInstanceMap"

-- | Gets an ordered list of @a@ from any @a@ that is of type 'UMap'.
asOrderedList :: UMap a -> [a]
asOrderedList = map fst . sortOn snd . map snd . Map.toList

-- | Our chunk databases. \Must contain all maps needed in an example.\
-- In turn, these maps must contain every chunk definition or concept 
-- used in its respective example, else an error is thrown.
data ChunkDB = CDB {
  -- CHUNKS
    symbolTable           :: SymbolMap
  , termTable             :: TermMap 
  , defTable              :: ConceptMap
  , _unitTable            :: UnitMap
  , _dataDefnTable        :: DatadefnMap
  , _insmodelTable        :: InsModelMap
  , _gendefTable          :: GendefMap
  , _theoryModelTable     :: TheoryModelMap
  , _conceptinsTable      :: ConceptInstanceMap
  -- NOT CHUNKS
  , _labelledcontentTable :: LabelledContentMap -- TODO: LabelledContent needs to be rebuilt. See JacquesCarette/Drasil#4023.
  , _refTable             :: ReferenceMap -- TODO: References need to be rebuilt. See JacquesCarette/Drasil#4022.
  , _traceTable           :: TraceMap
  , _refbyTable           :: RefbyMap
  }
makeLenses ''ChunkDB

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
cdb :: (Quantity q, MayHaveUnit q, Idea t, Concept c, IsUnit u) =>
    [q] -> [t] -> [c] -> [u] -> [DataDefinition] -> [InstanceModel] ->
    [GenDefn] -> [TheoryModel] -> [ConceptInstance] ->
    [LabelledContent] -> [Reference] -> ChunkDB
cdb s t c u d ins gd tm ci lc r = 
  CDB {
    -- CHUNKS
    symbolTable = symbolMap s,
    termTable = termMap t,
    defTable = conceptMap c,
    _unitTable = unitMap u,
    _dataDefnTable = idMap d,
    _insmodelTable = idMap ins,
    _gendefTable = idMap gd,
    _theoryModelTable = idMap tm,
    _conceptinsTable = idMap ci,
    _labelledcontentTable = idMap lc,
    -- NOT CHUNKS
    _traceTable = Map.empty,
    _refbyTable = Map.empty,
    _refTable = idMap r
  }

-- | Gets the units of a 'Quantity' as 'UnitDefn's.
collectUnits :: Quantity c => ChunkDB -> [c] -> [UnitDefn]
collectUnits m = map (unitWrapper . flip unitLookup (m ^. unitTable))
 . concatMap getUnits . mapMaybe (getUnitLup m)

-- | Trace a 'UID' to related 'UID's.
traceLookup :: UID -> TraceMap -> [UID]
traceLookup c = fromMaybe [] . Map.lookup c

-- | Translates a traceability map into a reference map.
generateRefbyMap :: TraceMap -> RefbyMap
generateRefbyMap = invert

-- | Trace a 'UID' to referenced 'UID's.
refbyLookup :: UID -> RefbyMap -> [UID]
refbyLookup c = fromMaybe [] . Map.lookup c
