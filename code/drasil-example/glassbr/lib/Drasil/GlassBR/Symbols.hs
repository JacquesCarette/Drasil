module Drasil.GlassBR.Symbols where

import Language.Drasil (QuantityDict, qw)
import Language.Drasil.Code (Mod(Mod), asVC)

import Drasil.GlassBR.ModuleDefs (allMods, implVars)
import Drasil.GlassBR.Unitals (inputs, outputs, specParamVals,
  symbolsWithDefns, unitless, tmSymbols, interps, derivedInputDataConstraints)

import Data.List ((\\))

symbolsForTable :: [QuantityDict]
symbolsForTable = inputs ++ outputs ++ tmSymbols ++ map qw specParamVals ++ 
  map qw symbolsWithDefns ++ map qw derivedInputDataConstraints ++ interps

  -- include all module functions as symbols
thisSymbols :: [QuantityDict]
thisSymbols = (map asVC (concatMap (\(Mod _ _ _ _ l) -> l) allMods)
  \\ symbolsForTable) ++ map qw implVars ++ symbolsForTable
