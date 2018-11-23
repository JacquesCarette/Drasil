{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.ChunkDB 
  ( ChunkDB, cdb
  , HasSymbolTable(..), symbolMap, symbLookup
  , HasTermTable(..), termLookup
  , HasDefinitionTable(..), conceptMap, defLookup
  , HasUnitTable(..), unitMap, collectUnits
  , HasUnitTable(..), unitMap, collectUnits, TraceMap,
  traceLookup, HasTraceTable(..), generateRefbyMap, RefbyMap,
  refbyLookup, HasRefbyTable(..)
  ) where

import Control.Lens ((^.), Lens', makeLenses)
import Data.Maybe (maybeToList)
import Data.List (nub, concat, union, elem, length, zip, head)
import Data.Tuple (fst, snd)
import Language.Drasil.UID (UID)
import Language.Drasil.Classes (Concept, ConceptDomain, HasUID(uid), Idea, IsUnit, HasDerivation(derivations)
  ,HasAdditionalNotes(getNotes), Quantity)
import Language.Drasil.Chunk.NamedIdea (IdeaDict, nw)
import Language.Drasil.Chunk.Quantity (QuantityDict, qw)
import Language.Drasil.Chunk.Concept (ConceptChunk, cw)
import Language.Drasil.Development.Unit(UnitDefn, MayHaveUnit(getUnit), unitWrapper)
import qualified Data.Map as Map
import Language.Drasil.Sentence.Extract(lnames)

-- The misnomers below are not actually a bad thing, we want to ensure data can't
-- be added to a map if it's not coming from a chunk, and there's no point confusing
-- what the map is for. One is for symbols + their units, and the others are for
-- what they state.

-- | A bit of a misnomer as it's really a map of all quantities, for retrieving
-- symbols and their units.
type SymbolMap  = Map.Map UID QuantityDict

-- | A map of all concepts, normally used for retrieving definitions.
type ConceptMap = Map.Map UID ConceptChunk

-- | A map of all the units used. Should be restricted to base units/synonyms.
type UnitMap = Map.Map UID UnitDefn

-- | Again a bit of a misnomer as it's really a map of all NamedIdeas.
-- Until these are built through automated means, there will
-- likely be some 'manual' duplication of terms as this map will contain all
-- quantities, concepts, etc.
type TermMap = Map.Map UID IdeaDict

-- The uid is label's uid
type TraceMap = Map.Map UID [UID]

-- The uid is label's uid
type RefbyMap = Map.Map UID [UID]

-- | Smart constructor for a 'SymbolMap'
symbolMap :: (Quantity c, MayHaveUnit c) => [c] -> SymbolMap
symbolMap = Map.fromList . map (\x -> (x ^. uid, qw x))

-- | Smart constructor for a 'TermMap'
termMap :: (Idea c) => [c] -> TermMap
termMap = Map.fromList . map (\x -> (x ^. uid, nw x))

-- | Smart constructor for a 'ConceptMap'
conceptMap :: (Concept c) => [c] -> ConceptMap
conceptMap = Map.fromList . map (\x -> (x ^. uid, cw x))

-- | Smart constructor for a 'UnitMap'
unitMap :: (IsUnit u, ConceptDomain u) => [u] -> UnitMap
unitMap = Map.fromList . map (\x -> (x ^. uid, unitWrapper x))

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

-- | Gets a unit if it exists, or Nothing.        
getUnitLup :: HasSymbolTable s => (HasUID c, MayHaveUnit c) => s -> c -> Maybe UnitDefn
getUnitLup m c = getUnit $ symbLookup (c ^. uid) (m ^. symbolTable)

-- | Looks up an uid in the term table. If nothing is found, an error is thrown
termLookup :: UID -> TermMap -> IdeaDict
termLookup c m = getT $ Map.lookup c m
  where getT = maybe (error $ "Term: " ++ c ++ " not found in TermMap") id

-- | Looks up a uid in the definition table. If nothing is found, an error is thrown.
defLookup :: UID -> ConceptMap -> ConceptChunk
defLookup u m = getC $ Map.lookup u m
  where getC = maybe (error $ "Concept: " ++ u ++ " not found in ConceptMap") id

-- | Our chunk databases. Should contain all the maps we will need.
data ChunkDB = CDB { _csymbs :: SymbolMap
                   , _cterms :: TermMap 
                   , _cdefs  :: ConceptMap
                   , _cunitDB :: UnitMap
                   , _ctrace :: TraceMap
                   , _crefby :: RefbyMap
                   } --TODO: Expand and add more databases
makeLenses ''ChunkDB

-- | Smart constructor for chunk databases. Takes a list of Quantities 
-- (for SymbolTable), NamedIdeas (for TermTable), Concepts (for DefinitionTable),
-- and Units (for UnitTable)
cdb :: (Quantity q, MayHaveUnit q, Idea t, Concept c, IsUnit u,
        ConceptDomain u) => [q] -> [t] -> [c] -> [u] -> TraceMap -> RefbyMap -> ChunkDB
cdb s t c u tc rfm = CDB (symbolMap s) (termMap t) (conceptMap c) (unitMap u) tc rfm

----------------------
instance HasSymbolTable     ChunkDB where symbolTable = csymbs
instance HasTermTable       ChunkDB where termTable   = cterms
instance HasDefinitionTable ChunkDB where defTable    = cdefs
instance HasUnitTable       ChunkDB where unitTable   = cunitDB
instance HasTraceTable      ChunkDB where traceTable  = ctrace
instance HasRefbyTable      ChunkDB where refbyTable  = crefby

collectUnits :: HasSymbolTable s => (Quantity c, MayHaveUnit c) => s -> [c] -> [UnitDefn]
collectUnits m symb = map unitWrapper $ concatMap maybeToList $ map (getUnitLup m) symb

traceLookup :: UID -> TraceMap -> [UID]
traceLookup c m = getT $ Map.lookup c m
  where getT = maybe [] id
 
invert :: (Ord k, Ord v) => Map.Map k [v] -> Map.Map v [k]
invert m = Map.fromListWith (++) pairs
    where pairs = [(v, [k]) | (k, vs) <- Map.toList m, v <- vs]
 
generateRefbyMap :: TraceMap  -> RefbyMap
generateRefbyMap tm = invert tm

{--lookupRefMap :: UID -> Label -> LabelMap -> (UID, [Label])
lookupRefMap a b lm = (b ^. uid, [labelLookup a lm])--}

refbyLookup :: UID -> RefbyMap -> [UID]
refbyLookup c m = getT $ Map.lookup c m
  where getT = maybe [] id
