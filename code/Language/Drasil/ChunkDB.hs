module Language.Drasil.ChunkDB 
  ( SymbolMap, symbolMap, symbLookup, getUnitLup 
  , ChunkDB(..), cdb
  ) where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Unit

import Control.Lens ((^.))
import qualified Data.Map as Map

import Prelude hiding (id)

type SymbolMap = Map.Map String QWrapper

symbolMap :: (Quantity c) => [c] -> SymbolMap
symbolMap cs = Map.fromList (map (\x -> ((x ^. id), qs x)) cs)

-- | Looks up an id in the symbol table. If nothing is found, an error is thrown
symbLookup :: (Chunk c) => c -> SymbolMap -> QWrapper
symbLookup c m = let lookC = Map.lookup (c ^. id) m in
                 getS lookC
  where getS (Just x) = x
        getS Nothing = error $ "Symbol: " ++ (c ^. id) ++ " not found in SymbolMap"

-- | Gets a unit if it exists, or Nothing.        
getUnitLup :: (Chunk c) => c -> SymbolMap -> Maybe UnitDefn
getUnitLup c m = let lookC = symbLookup c m in
                 getUnit lookC

-- | Our chunk databases. Should contain all the maps we will need.
data ChunkDB = CDB { symbs :: SymbolMap } --TODO: Expand and add more databases

cdb :: SymbolMap -> ChunkDB
cdb = CDB
  