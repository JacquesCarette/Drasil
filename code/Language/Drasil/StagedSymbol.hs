module Language.Drasil.StagedSymbol where

import Language.Drasil.Symbol

-- TODO: More will likely be added here as we figure it out.

-- FIXME: Stages should be more abstract and meaningful.
-- | Symbols can have different representations based on the stage 
-- of generation currently taking place.
data StagedSymbol = Equational Symbol
                  | Code Symbol

-- FIXME: More fine-grained stages.
-- | Stages correspond to what we're trying to look up. They range from abstract
-- to concrete.                  
data Stage = Theoretical
           | Implementation

{- Note: Keep stages separate from StagedSymbols for lookup purposes, as we may
-- have documents which look up both stages of a symbol and show them 
-- side-by-side or one after another (think LPM). -}

instance Show Stage where
  show Theoretical    = "Theory Stage"
  show Implementation = "Implementation Stage"

getSymbolAtStage :: Stage -> [StagedSymbol] -> Symbol
getSymbolAtStage s [] = error $ "No symbol found for " ++ show s
getSymbolAtStage Theoretical    (Equational x : xs) = x
getSymbolAtStage Implementation (Code x       : xs) = x
getSymbolAtStage s (x:xs) = getSymbolAtStage s xs
