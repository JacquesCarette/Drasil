module Drasil.GlassBR.Symbols where

import Language.Drasil (QuantityDict, qw)
import Language.Drasil.Code (Mod(Mod), asVC, asVC')

import Drasil.GlassBR.IMods (gbrIMods)
import Drasil.GlassBR.ModuleDefs (allMods, implVars, interpY, interpZ)
import Drasil.GlassBR.Unitals (gbInputDataConstraints, gbInputs, gbOutputs,
    gBRSpecParamVals, glassBRSymbols, glassBRSymbolsWithDefns, glassBRUnitless, gbTMSymbols)

import Data.List ((\\))

symbolsForTable :: [QuantityDict]
symbolsForTable = gbInputs ++ gbOutputs ++ gbTMSymbols ++ (map qw gBRSpecParamVals) ++ 
  (map qw glassBRSymbolsWithDefns) ++ (map qw glassBRSymbols) ++
  (map qw glassBRUnitless) ++ (map qw gbInputDataConstraints) ++
  (map asVC' [interpY, interpZ]) 

thisSymbols :: [QuantityDict]
thisSymbols = (map qw gbrIMods) 
  -- include all module functions as symbols
  ++ (map asVC (concatMap (\(Mod _ l) -> l) allMods) \\ symbolsForTable)
  ++ map qw implVars ++ symbolsForTable
