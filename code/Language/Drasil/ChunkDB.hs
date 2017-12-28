module Language.Drasil.ChunkDB 
  ( symbolMap, symbLookup, getUnitLup, elements, termTable
  , ChunkDB(..), cdb, HasSymbolTable(..), termLookup, HasTermTable
  ) where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Wrapper
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
type SymbolMap  = Map.Map String QWrapper

type ConceptMap = Map.Map String CWrapper

-- | Again a bit of a misnomer as it's really a map of all NamedIdeas.
-- Until these are built through automated means, there will
-- likely be some 'manual' duplication of terms as this map will contain all
-- quantities, concepts, etc.
type TermMap = Map.Map String NWrapper

-- | Smart constructor for a 'SymbolMap'
symbolMap :: (Quantity c) => [c] -> SymbolMap
symbolMap cs = Map.fromList (map (\x -> ((x ^. id), qs x)) cs)

-- | Smart constructor for a 'TermMap'
termMap :: (NamedIdea c) => [c] -> TermMap
termMap ts = Map.fromList (map (\x -> ((x ^. id), nw x)) ts)

conceptMap :: (Concept c) => [c] -> ConceptMap
conceptMap cs = Map.fromList (map (\x -> ((x ^. id), cw x)) cs)

-- | Get all the elements of one of our tables
elements :: Map.Map k a -> [a]
elements m = Map.elems m

-- | Looks up an id in the symbol table. If nothing is found, an error is thrown
symbLookup :: (Chunk c) => c -> SymbolMap -> QWrapper
symbLookup c m = let lookC = Map.lookup (c ^. id) m in
                 getS lookC
  where getS (Just x) = x
        getS Nothing = error $ "Symbol: " ++ (c ^. id) ++ " not found in SymbolMap"

-- | Gets a unit if it exists, or Nothing.        
getUnitLup :: HasSymbolTable s => (Chunk c) => c -> s -> Maybe UnitDefn
getUnitLup c m = let lookC = symbLookup c (m ^. symbolTable) in
                 getUnit lookC
                 
termLookup :: (Chunk c) => c -> SymbolMap -> QWrapper
termLookup c m = let lookC = Map.lookup (c ^. id) m in
                 getT lookC
  where getT (Just x) = x
        getT Nothing  = error $ "Term: " ++ (c ^. id) ++ " not found in TermMap"

-- | Our chunk databases. Should contain all the maps we will need.
data ChunkDB = CDB { symbs :: SymbolMap
                   , terms :: TermMap 
                   , defs  :: ConceptMap } --TODO: Expand and add more databases

cdb :: (Quantity q, NamedIdea t, Concept c) => [q] -> [t] -> [c] -> ChunkDB
cdb s t c = CDB (symbolMap s) (termMap t) (conceptMap c)

class HasSymbolTable s where
  symbolTable :: Simple Lens s SymbolMap

instance HasSymbolTable ChunkDB where
  symbolTable f (CDB s t c) = fmap (\x -> CDB x t c) (f s)

class HasTermTable s where
  termTable :: Simple Lens s TermMap
  
instance HasTermTable ChunkDB where
  termTable f (CDB s t c) = fmap (\x -> CDB s x c) (f t)

class HasDefinitionTable s where
  defTable :: Simple Lens s ConceptMap

instance HasDefinitionTable ChunkDB where
  defTable f (CDB s t c) = fmap (\x -> CDB s t x) (f c)

