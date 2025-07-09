module Drasil.GlassBR.Symbols where

import Language.Drasil (QuantityDict, qw, cnstrw)
import Language.Drasil.Code (Mod(Mod), asVC)

import Drasil.GlassBR.ModuleDefs (allMods, implVars)
import Drasil.GlassBR.Unitals (specParamVals, modElas,
  tmSymbols, interps, derivedInputDataConstraints, unitless, probBr,
  stressDistFac, nomThick, sdVector, inputsWUnitsUncrtn, inputsWUncrtn,
  glassTypeCon, unitalSymbols, unitarySymbols)

import Data.List ((\\))

symbolsForSymbolTable :: [QuantityDict]
symbolsForSymbolTable = symbolsForTermTable ++ map qw unitalSymbols ++
  map qw unitarySymbols ++ unitless ++ map qw [probBr, stressDistFac,
  nomThick, cnstrw glassTypeCon] ++ map qw derivedInputDataConstraints

symbolsForTermTable :: [QuantityDict]
symbolsForTermTable = map qw inputsWUnitsUncrtn ++ map qw inputsWUncrtn ++
  map qw sdVector ++ tmSymbols ++ map qw specParamVals ++ 
  [qw modElas] ++ interps

  -- include all module functions as symbols
thisSymbols :: [QuantityDict]
thisSymbols = (map asVC (concatMap (\(Mod _ _ _ _ l) -> l) allMods)
  \\ symbolsForSymbolTable) ++ map qw implVars ++ symbolsForSymbolTable
  
thisTerms :: [QuantityDict]
thisTerms = (map asVC (concatMap (\(Mod _ _ _ _ l) -> l) allMods)
  \\ symbolsForTermTable) ++ map qw implVars ++ symbolsForTermTable
