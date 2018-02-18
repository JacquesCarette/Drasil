module Language.Drasil.Chunk.SymbolForm
  (HasSymbol(..), Stage(..), eqSymb, codeSymb, hasStageSymbol) where

import Language.Drasil.Symbol (Symbol(Empty))

import Prelude hiding (id)

-- | A HasSymbol is anything which has a Symbol
class HasSymbol c where
  -- | Provides the Symbol --  for a particular stage of generation
  symbol  :: Stage -> c -> Symbol
  
-- FIXME: More fine-grained stages.
-- | Stages correspond to what we're trying to look up. They range from abstract
-- to concrete.                  
data Stage = Equational -- AKA Theoretical / Abstract-design
           | Implementation -- AKA Implementation / Detailed-design
  deriving (Eq, Ord)

{- Note: Keep stages separate from StagedSymbols for lookup purposes, as we may
-- have documents which look up both stages of a symbol and show them 
-- side-by-side or one after another (think LPM). -}

-- | For better error messages.
instance Show Stage where
  show Equational     = "Theoretical stage"
  show Implementation = "Implementation Stage"

-- | Helper function for getting a symbol in the Equational Stage
eqSymb :: HasSymbol q => q -> Symbol
eqSymb = symbol Equational

-- | Helper function for getting a symbol in the Implementation Stage
codeSymb :: HasSymbol q => q -> Symbol
codeSymb = symbol Implementation

-- | Is a Stage symbol real or Empty?
hasStageSymbol :: HasSymbol q => q -> Stage -> Bool
hasStageSymbol q st = symbol st q /= Empty
