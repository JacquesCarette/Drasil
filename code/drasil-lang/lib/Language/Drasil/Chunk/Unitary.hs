-- | Defines chunks to add units to a quantity. Similar to 'UnitalChunk'.
module Language.Drasil.Chunk.Unitary (
  -- * Chunk Types
  Unitary(..), 
  ) where

import Language.Drasil.Classes (Quantity)
import Language.Drasil.Chunk.UnitDefn (UnitDefn)

-- | A Unitary is a 'Quantity' that __must__ have a unit.
class (Quantity c) => Unitary c where
  unit :: c -> UnitDefn

