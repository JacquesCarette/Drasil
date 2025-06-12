{-# LANGUAGE TemplateHaskell #-}
-- | Defines types and functions to create a chunk database within Drasil.

-- Changes to ChunkDB should be reflected in the 'Creating Your Project 
-- in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil
module Database.Drasil.ChunkDB (
  -- * Types
  -- ** 'ChunkDB'
  -- | Main database type
  ChunkDB(CDB, symbolTable, termTable, conceptChunkTable),
  TermAbbr(..),
  -- ** Maps
  -- | Exported for external use.
  RefbyMap, TraceMap, UMap,
  -- * Functions
  -- ** Constructors
  cdb, idMap, termMap, conceptMap, traceMap, generateRefbyMap, -- idMap, termMap for docLang
  -- ** Lookup Functions
  asOrderedList, collectUnits,
  termResolve, termResolve', defResolve, symbResolve,
  traceLookup, refbyLookup,
  datadefnLookup, insmodelLookup, gendefLookup, theoryModelLookup,
  conceptinsLookup, refResolve,
  -- ** Lenses
  unitTable, traceTable, refbyTable, citationTable,
  dataDefnTable, insmodelTable, gendefTable, theoryModelTable,
  conceptinsTable, labelledcontentTable, refTable
) where

import Language.Drasil (HasUID(..), UID, Quantity, MayHaveUnit(..), Idea (..),
  QuantityDict, IdeaDict, Concept, ConceptChunk, IsUnit(..), UnitDefn,
  Reference, ConceptInstance, LabelledContent, Citation,
  qw, nw, cw, unitWrapper, NP, NamedIdea(..))
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)

import Control.Lens ((^.), makeLenses)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map as Map
import Utils.Drasil (invert)
import Debug.Trace (trace)
import Data.Drasil.Concepts.Documentation (doccon, srsDomains)
import Data.Drasil.Software.Products (prodtcon)
import Data.Drasil.Concepts.Computation (algorithm, compcon)
import Data.Drasil.Concepts.Software (errMsg, program)
import Data.Drasil.Concepts.Math (mathcon)
import Data.Drasil.Concepts.Education (educon)

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
-- | Citation map.
type CitationMap = UMap Citation

-- | General chunk database map constructor. Creates a 'UMap' from a function
-- that converts something with 'UID's into another type and a list of something
-- with 'UID's.
cdbMap :: HasUID a => String -> (a -> b) -> [a] -> Map.Map UID (b, Int)
cdbMap mn f rawChunks = Map.fromListWithKey preferNew kvs
  where
    kvs = zipWith (\i c -> (c ^. uid, (f c, i))) [1..] rawChunks
    preferNew key new _ = trace ("'" ++ show key ++ "' is inserted twice in '" ++ mn ++ "'!") new

-- | Smart constructor for a 'SymbolMap'.
symbolMap :: (Quantity c, MayHaveUnit c) => [c] -> SymbolMap
symbolMap = cdbMap "SymbolMap" qw

-- | Smart constructor for a 'TermMap'.
termMap :: (Idea c) => [c] -> TermMap
termMap = cdbMap "TermMap" nw

-- | Smart constructor for a 'ConceptMap'.
conceptMap :: (Concept c) => [c] -> ConceptMap
conceptMap = cdbMap "ConceptMap" cw

-- | Smart constructor for a 'UnitMap'.
unitMap :: (IsUnit u) => [u] -> UnitMap
unitMap = cdbMap "UnitMap" unitWrapper

-- | General smart constructor for making a 'UMap' out of anything that has a 'UID'. 
idMap :: HasUID a => String -> [a] -> Map.Map UID (a, Int)
idMap mn = cdbMap mn id

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

data TermAbbr = TermAbbr { longForm :: NP, shortForm :: Maybe String }

-- | Search for _any_ chunk that is an instance of 'Idea' and process its "term"
-- and abbreviation.
termResolve :: (NP -> Maybe String -> c) -> ChunkDB -> UID -> c
termResolve f db trg
  | (Just (c, _)) <- Map.lookup trg (termTable db) = go c
  | (Just (c, _)) <- Map.lookup trg (symbolTable db)       = go c
  | (Just (c, _)) <- Map.lookup trg (conceptChunkTable db) = go c
  | (Just (c, _)) <- Map.lookup trg (_unitTable db)        = go c
  | (Just (c, _)) <- Map.lookup trg (_dataDefnTable db)    = go c
  | (Just (c, _)) <- Map.lookup trg (_insmodelTable db)    = go c
  | (Just (c, _)) <- Map.lookup trg (_gendefTable db)      = go c
  | (Just (c, _)) <- Map.lookup trg (_theoryModelTable db) = go c
  | (Just (c, _)) <- Map.lookup trg (_conceptinsTable db)  = go c
  | otherwise = error $ "Term: " ++ show trg ++ " not found in TermMap"
  where go c = f (c ^. term) (getA c)

-- | Find a chunks "term" and abbreviation, if it exists.
termResolve' :: ChunkDB -> UID -> TermAbbr
termResolve' = termResolve TermAbbr

-- | Looks up a 'UID' in the reference table from the 'ChunkDB'. If nothing is found, an error is thrown.
refResolve :: UID -> ReferenceMap -> Reference
refResolve = uMapLookup "Reference" "ReferenceMap"

-- | Looks up a 'UID' in the unit table. If nothing is found, an error is thrown.
unitLookup :: UID -> UnitMap -> UnitDefn
unitLookup = uMapLookup "Unit" "UnitMap"

-- | Looks up a 'UID' in the definition table from the 'ChunkDB'. If nothing is found, an error is thrown.
defResolve :: ChunkDB -> UID -> ConceptChunk
defResolve m x = uMapLookup "Concept" "ConceptMap" x $ conceptChunkTable m

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
  , conceptChunkTable     :: ConceptMap
  , _unitTable            :: UnitMap
  , _dataDefnTable        :: DatadefnMap
  , _insmodelTable        :: InsModelMap
  , _gendefTable          :: GendefMap
  , _theoryModelTable     :: TheoryModelMap
  , _conceptinsTable      :: ConceptInstanceMap
  , _citationTable        :: CitationMap
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


addCdb :: ChunkDB -> ChunkDB -> ChunkDB
addCdb cdb1 cdb2 =
  CDB {
    -- CHUNKS
    symbolTable           = concatCdbMap "SymbolMap" (symbolTable cdb1) (symbolTable cdb2),
    termTable             = concatCdbMap "TermMap" (termTable cdb1) (termTable cdb2),
    conceptChunkTable     = concatCdbMap "ConceptMap" (conceptChunkTable cdb1) (conceptChunkTable cdb2),
    _unitTable            = concatCdbMap "UnitMap" (_unitTable cdb1) (_unitTable cdb2),
    _dataDefnTable        = concatCdbMap "" (_dataDefnTable cdb1) (_dataDefnTable cdb2),
    _insmodelTable        = concatCdbMap "" (_insmodelTable cdb1) (_insmodelTable cdb2),
    _gendefTable          = concatCdbMap "" (_gendefTable cdb1) (_gendefTable cdb2),
    _theoryModelTable     = concatCdbMap "" (_theoryModelTable cdb1) (_theoryModelTable cdb2),
    _conceptinsTable      = concatCdbMap "" (_conceptinsTable cdb1) (_conceptinsTable cdb2),
    _citationTable        = concatCdbMap "" (_citationTable cdb1) (_citationTable cdb2),
    -- NOT CHUNKS
    _labelledcontentTable = concatCdbMap "" (_labelledcontentTable cdb1) (_labelledcontentTable cdb2),
    _traceTable           = concatCdbMap "" (_traceTable cdb1) (_traceTable cdb2),
    _refbyTable           = concatCdbMap "" (_refbyTable cdb1) (_refbyTable cdb2),
    _refTable             = concatCdbMap "" (_refTable cdb1) (_refTable cdb2)
  }
  where
    concatCdbMap mn = Map.unionWithKey (preferNew mn)
    preferNew mn key new _ = trace ("'" ++ show key ++ "' is inserted twice in '" ++ mn ++ "' while adding to basis!") new

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

basisIdeaDicts :: [IdeaDict]
basisIdeaDicts =
  -- Actual IdeaDicts
  doccon ++ prodtcon ++ educon ++ compcon ++
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
