module Language.Drasil.Symbol.Helpers(eqSymb, codeSymb, hasStageSymbol) where

import Language.Drasil.Classes (HasSymbol(symbol))
import Language.Drasil.Symbol (Symbol(Empty))
import Language.Drasil.Stages (Stage(Equational,Implementation))

-- | Helper function for getting a symbol in the Equational Stage
eqSymb :: HasSymbol q => q -> Symbol
eqSymb c = symbol c Equational

-- | Helper function for getting a symbol in the Implementation Stage
codeSymb :: HasSymbol q => q -> Symbol
codeSymb c = symbol c Implementation

-- | Is a Stage symbol real or Empty?
hasStageSymbol :: HasSymbol q => q -> Stage -> Bool
hasStageSymbol q st = symbol q st /= Empty
