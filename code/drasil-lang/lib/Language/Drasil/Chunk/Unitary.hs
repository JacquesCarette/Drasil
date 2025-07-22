-- | Defines chunks to add units to a quantity. Similar to 'UnitalChunk'.
module Language.Drasil.Chunk.Unitary (
  -- * Chunk Types
  Unitary(..), 
  -- * Helpers
  unit_symb
  ) where

import Language.Drasil.Classes (Quantity, usymb)
import Language.Drasil.Chunk.UnitDefn (UnitDefn)
import Language.Drasil.UnitLang (USymb)

-- | A Unitary is a 'Quantity' that __must__ have a unit.
class (Quantity c) => Unitary c where
  unit :: c -> UnitDefn

-- | Helper for getting the unit's 'Symbol' from a chunk, 
-- as opposed to the symbols of the chunk itself.
unit_symb :: (Unitary c) => c -> USymb
unit_symb c = usymb $ unit c