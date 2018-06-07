{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.ChunkDB 
  ( ChunkDB, cdb
  , HasSymbolTable(..), symbolMap, symbLookup, getUnitLup
  , HasTermTable(..), termLookup
  , HasDefinitionTable(..), conceptMap
  , HasUnitTable(..), unitMap
  ) where

import Language.Drasil.UID (UID)
import Language.Drasil.Classes (HasUID(uid), Idea, Concept, DOM, IsUnit)
import Language.Drasil.Chunk.NamedIdea (IdeaDict, nw)
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Concept 
import Language.Drasil.Unit (unitWrapper, UnitDefn)

import Control.Lens ((^.), Lens', makeLenses)
import qualified Data.Map as Map

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

-- | Smart constructor for a 'SymbolMap'
symbolMap :: (Quantity c) => [c] -> SymbolMap
symbolMap = Map.fromList . map (\x -> (x ^. uid, qw x))

-- | Smart constructor for a 'TermMap'
termMap :: (Idea c) => [c] -> TermMap
termMap = Map.fromList . map (\x -> (x ^. uid, nw x))

-- | Smart constructor for a 'ConceptMap'
conceptMap :: (Concept c, DOM c ~ ConceptChunk) => [c] -> ConceptMap
conceptMap = Map.fromList . map (\x -> (x ^. uid, cw x))

-- | Smart constructor for a 'UnitMap'
unitMap :: (IsUnit u, DOM u ~ ConceptChunk) => [u] -> UnitMap
unitMap = Map.fromList . map (\x -> (x ^. uid, unitWrapper x))

-- | Looks up an uid in the symbol table. If nothing is found, an error is thrown
symbLookup :: UID -> SymbolMap -> QuantityDict
symbLookup c m = getS $ Map.lookup c m
  where getS (Just x) = x
        getS Nothing = error $ "Symbol: " ++ c ++ " not found in SymbolMap"

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

-- | Gets a unit if it exists, or Nothing.        
getUnitLup :: HasSymbolTable s => (HasUID c) => c -> s -> Maybe UnitDefn
getUnitLup c m = getUnit $ symbLookup (c ^. uid) (m ^. symbolTable)

-- | Looks up an uid in the term table. If nothing is found, an error is thrown
termLookup :: (HasUID c) => c -> TermMap -> IdeaDict
termLookup c m = getT $ Map.lookup (c ^. uid) m
  where getT (Just x) = x
        getT Nothing  = error $ "Term: " ++ (c ^. uid) ++ " not found in TermMap"

-- | Looks up a uid in the definition table. If nothing is found, an error is thrown.
defLookup :: UID -> ConceptMap -> ConceptChunk
defLookup u m = getC $ Map.lookup u m
  where getC = maybe (error $ "Concept: " ++ u ++ " not found in ConceptMap") id

-- | Our chunk databases. Should contain all the maps we will need.
data ChunkDB = CDB { _csymbs :: SymbolMap
                   , _cterms :: TermMap 
                   , _cdefs  :: ConceptMap
                   , _cunitDB :: UnitMap
                   } --TODO: Expand and add more databases
makeLenses ''ChunkDB

-- | Smart constructor for chunk databases. Takes a list of Quantities 
-- (for SymbolTable), NamedIdeas (for TermTable), Concepts (for DefinitionTable),
-- and Units (for UnitTable)
cdb :: (Quantity q, Idea t, Concept c, IsUnit u,
        DOM c ~ ConceptChunk, DOM u ~ ConceptChunk) => [q] -> [t] -> [c] -> [u] -> ChunkDB
cdb s t c u = CDB (symbolMap s) (termMap t) (conceptMap c) (unitMap u)

----------------------
instance HasSymbolTable     ChunkDB where symbolTable = csymbs
instance HasTermTable       ChunkDB where termTable   = cterms
instance HasDefinitionTable ChunkDB where defTable    = cdefs
instance HasUnitTable       ChunkDB where unitTable   = cunitDB

