module Language.Drasil.ChunkDB 
  ( elements
  , ChunkDB(..), cdb
  , HasSymbolTable(..), symbolMap, symbLookup, getUnitLup
  , HasTermTable(..), termLookup
  , HasDefinitionTable(..), conceptMap
  , HasUnitTable(..), unitMap
  ) where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept 
import Language.Drasil.Unit

import Control.Lens ((^.), Simple, Lens)
import qualified Data.Map as Map

import Prelude hiding (id)

-- The misnomers below are not actually a bad thing, we want to ensure data can't
-- be added to a map if it's not coming from a chunk, and there's no point confusing
-- what the map is for. One is for symbols + their units, and the others are for
-- what they state.

-- | A bit of a misnomer as it's really a map of all quantities, for retrieving
-- symbols and their units.
type SymbolMap  = Map.Map String QuantityDict

-- | A map of all concepts, normally used for retrieving definitions.
type ConceptMap = Map.Map String ConceptChunk

-- | A map of all the units used. Should be restricted to base units/synonyms.
type UnitMap = Map.Map String UnitDefn

-- | Again a bit of a misnomer as it's really a map of all NamedIdeas.
-- Until these are built through automated means, there will
-- likely be some 'manual' duplication of terms as this map will contain all
-- quantities, concepts, etc.
type TermMap = Map.Map String IdeaDict

-- | Smart constructor for a 'SymbolMap'
symbolMap :: (Quantity c) => [c] -> SymbolMap
symbolMap cs = Map.fromList (map (\x -> (x ^. id, qw x)) cs)

-- | Smart constructor for a 'TermMap'
termMap :: (Idea c) => [c] -> TermMap
termMap ts = Map.fromList (map (\x -> (x ^. id, nw x)) ts)

-- | Smart constructor for a 'ConceptMap'
conceptMap :: (Concept c) => [c] -> ConceptMap
conceptMap cs = Map.fromList (map (\x -> (x ^. id, cw x)) cs)

-- | Smart constructor for a 'UnitMap'
unitMap :: (Unit u) => [u] -> UnitMap
unitMap us = Map.fromList (map (\x -> (x ^. id, unitWrapper x)) us)

-- | Get all the elements of one of our tables
elements :: Map.Map k a -> [a]
elements m = Map.elems m

-- | Looks up an id in the symbol table. If nothing is found, an error is thrown
symbLookup :: (Chunk c) => c -> SymbolMap -> QuantityDict
symbLookup c m = let lookC = Map.lookup (c ^. id) m in
                 getS lookC
  where getS (Just x) = x
        getS Nothing = error $ "Symbol: " ++ (c ^. id) ++ " not found in SymbolMap"

-- | Gets a unit if it exists, or Nothing.        
getUnitLup :: HasSymbolTable s => (Chunk c) => c -> s -> Maybe UnitDefn
getUnitLup c m = let lookC = symbLookup c (m ^. symbolTable) in
                 getUnit lookC

-- | Looks up an id in the term table. If nothing is found, an error is thrown
termLookup :: (Chunk c) => c -> TermMap -> IdeaDict
termLookup c m = let lookC = Map.lookup (c ^. id) m in
                 getT lookC
  where getT (Just x) = x
        getT Nothing  = error $ "Term: " ++ (c ^. id) ++ " not found in TermMap"

-- | Our chunk databases. Should contain all the maps we will need.
data ChunkDB = CDB { symbs :: SymbolMap
                   , terms :: TermMap 
                   , defs  :: ConceptMap
                   , unitDB :: UnitMap} --TODO: Expand and add more databases

-- | Smart constructor for chunk databases. Takes a list of Quantities 
-- (for SymbolTable), NamedIdeas (for TermTable), Concepts (for DefinitionTable),
-- and Units (for UnitTable)
cdb :: (Quantity q, Idea t, Concept c, Unit u) => 
  [q] -> [t] -> [c] -> [u] -> ChunkDB
cdb s t c u = CDB (symbolMap s) (termMap t) (conceptMap c) (unitMap u)

--- SYMBOL TABLE ---
class HasSymbolTable s where
  symbolTable :: Simple Lens s SymbolMap

instance HasSymbolTable ChunkDB where
  symbolTable f (CDB s t c u) = fmap (\x -> CDB x t c u) (f s)

--- TERM TABLE ---
class HasTermTable s where
  termTable :: Simple Lens s TermMap
  
instance HasTermTable ChunkDB where
  termTable f (CDB s t c u) = fmap (\x -> CDB s x c u) (f t)

--- DEFINITION TABLE ---
class HasDefinitionTable s where
  defTable :: Simple Lens s ConceptMap

instance HasDefinitionTable ChunkDB where
  defTable f (CDB s t c u) = fmap (\x -> CDB s t x u) (f c)

--- UNIT TABLE ---
class HasUnitTable s where
  unitTable :: Simple Lens s UnitMap

instance HasUnitTable ChunkDB where
  unitTable f (CDB s t c u) = fmap (\x -> CDB s t c x) (f u)

