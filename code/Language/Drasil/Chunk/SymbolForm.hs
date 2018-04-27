module Language.Drasil.Chunk.SymbolForm
  (HasSymbol(..), Stage(..), eqSymb, codeSymb, hasStageSymbol) where

import Language.Drasil.Symbol (Symbol(Empty))

-- | A HasSymbol is anything which has a Symbol
class HasSymbol c where
  -- | Provides the Symbol --  for a particular stage of generation
  symbol  :: c -> Stage -> Symbol
  
-- FIXME: More fine-grained stages.
-- | Stages correspond to what we're trying to look up. They range from abstract
-- to concrete.                  
data Stage = Equational -- AKA Theoretical / Abstract-design
           | Implementation -- AKA Implementation / Detailed-design

{- Note: Keep stages separate from StagedSymbols for lookup purposes, as we may
   have documents which look up both stages of a symbol and show them 
   side-by-side or one after another. -}

-- | For better error messages.
instance Show Stage where
  show Equational     = "Theoretical stage"
  show Implementation = "Implementation Stage"

-- | Helper function for getting a symbol in the Equational Stage
eqSymb :: HasSymbol q => q -> Symbol
eqSymb = \c -> symbol c Equational

-- | Helper function for getting a symbol in the Implementation Stage
codeSymb :: HasSymbol q => q -> Symbol
codeSymb = \c -> symbol c Implementation

-- | Is a Stage symbol real or Empty?
hasStageSymbol :: HasSymbol q => q -> Stage -> Bool
hasStageSymbol q st = symbol q st /= Empty
