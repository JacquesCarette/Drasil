module Drasil.GlassBR.Symbols where

import Language.Drasil (QuantityDict, qw)
import Language.Drasil.Code (Mod(Mod), asVC)

import Drasil.GlassBR.ModuleDefs (allMods, implVars)
import Drasil.GlassBR.Unitals (inputs, outputs, specParamVals, symbols,
  symbolsWithDefns, tmSymbols, interps, derivedInputDataConstraints, unitless)

import Data.List ((\\))

symbolsForSymbolTable :: [QuantityDict]
symbolsForSymbolTable = symbolsForTermTable ++ map qw symbols ++
  unitless ++ map qw derivedInputDataConstraints

symbolsForTermTable :: [QuantityDict]
symbolsForTermTable = inputs ++ outputs ++ tmSymbols ++ map qw specParamVals ++ 
  map qw symbolsWithDefns ++ interps

  -- include all module functions as symbols
thisSymbols :: [QuantityDict]
thisSymbols = (map asVC (concatMap (\(Mod _ _ _ _ l) -> l) allMods)
  \\ symbolsForSymbolTable) ++ map qw implVars ++ symbolsForSymbolTable
  
thisTerms :: [QuantityDict]
thisTerms = (map asVC (concatMap (\(Mod _ _ _ _ l) -> l) allMods)
  \\ symbolsForTermTable) ++ map qw implVars ++ symbolsForTermTable
