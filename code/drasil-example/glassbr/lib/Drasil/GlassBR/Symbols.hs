module Drasil.GlassBR.Symbols where

import Control.Lens ((^.))

import Language.Drasil (QuantityDict, qw, cnstrw, DefinesQuantity(defLhs))
import Language.Drasil.Code (Mod(Mod), asVC)

import Drasil.GlassBR.ModuleDefs (allMods, implVars)
import Drasil.GlassBR.Unitals (specParamVals, symbols, symbolsWithDefns,
  tmSymbols, interps, derivedInputDataConstraints, unitless, probBr,
  stressDistFac, nomThick, sdVector, inputsWUnitsUncrtn, inputsWUncrtn, glassTypeCon)

import Data.List ((\\))

symbolsForSymbolTable :: [QuantityDict]
symbolsForSymbolTable = symbolsForTermTable ++ map qw symbols ++
  unitless ++ map qw [probBr, stressDistFac, nomThick, cnstrw glassTypeCon] ++
  map qw derivedInputDataConstraints

symbolsForTermTable :: [QuantityDict]
symbolsForTermTable = map qw inputsWUnitsUncrtn ++ map qw inputsWUncrtn ++
  map qw sdVector ++ tmSymbols ++ map (^. defLhs) specParamVals ++ 
  map qw symbolsWithDefns ++ interps

  -- include all module functions as symbols
thisSymbols :: [QuantityDict]
thisSymbols = (map asVC (concatMap (\(Mod _ _ _ _ l) -> l) allMods)
  \\ symbolsForSymbolTable) ++ map qw implVars ++ symbolsForSymbolTable
  
thisTerms :: [QuantityDict]
thisTerms = (map asVC (concatMap (\(Mod _ _ _ _ l) -> l) allMods)
  \\ symbolsForTermTable) ++ map qw implVars ++ symbolsForTermTable
