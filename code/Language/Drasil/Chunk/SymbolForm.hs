module Language.Drasil.Chunk.SymbolForm
  (HasSymbol(..), Stage(..), eqSymb, codeSymb, hasStageSymbol) where

import Language.Drasil.Symbol (Symbol(Empty), Stage(Equational,Implementation))

-- | A HasSymbol is anything which has a Symbol
class HasSymbol c where
  -- | Provides the Symbol --  for a particular stage of generation
  symbol  :: c -> Stage -> Symbol
  
-- | Helper function for getting a symbol in the Equational Stage
eqSymb :: HasSymbol q => q -> Symbol
eqSymb = \c -> symbol c Equational

-- | Helper function for getting a symbol in the Implementation Stage
codeSymb :: HasSymbol q => q -> Symbol
codeSymb = \c -> symbol c Implementation

-- | Is a Stage symbol real or Empty?
hasStageSymbol :: HasSymbol q => q -> Stage -> Bool
hasStageSymbol q st = symbol q st /= Empty
