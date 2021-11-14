-- | Defining the core classes which represent knowledge-about-knowledge.
module Language.Drasil.Classes.Core (
  HasSymbol(symbol)
) where

import Language.Drasil.Stages (Stage)
import Language.Drasil.Symbol (Symbol)

-- | A HasSymbol is anything which has a 'Symbol'.
class HasSymbol c where
  -- | Provides the 'Symbol' for a particular stage of generation.
  symbol  :: c -> Stage -> Symbol
