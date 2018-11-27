module Language.Drasil.Symbol.Helpers(eqSymb, codeSymb, hasStageSymbol, sCurlyBrSymb) where

import Language.Drasil.Classes (HasSymbol(symbol))
import Language.Drasil.Symbol (Symbol(Empty, Concat, Special))
import Language.Drasil.Stages (Stage(Equational,Implementation))
import Language.Drasil.Unicode (Special(CurlyBrClose, CurlyBrOpen))

-- | Helper function for getting a symbol in the Equational Stage
eqSymb :: HasSymbol q => q -> Symbol
eqSymb c = symbol c Equational

-- | Helper function for getting a symbol in the Implementation Stage
codeSymb :: HasSymbol q => q -> Symbol
codeSymb c = symbol c Implementation

-- | Is a Stage symbol real or Empty?
hasStageSymbol :: HasSymbol q => q -> Stage -> Bool
hasStageSymbol q st = symbol q st /= Empty

-- | Helper for adding {} around a symbol (used for coordinates).
sCurlyBrSymb :: Symbol -> Symbol
sCurlyBrSymb x = Concat [Special CurlyBrOpen, x, Special CurlyBrClose]
