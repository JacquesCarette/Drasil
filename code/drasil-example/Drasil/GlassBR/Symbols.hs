module Drasil.GlassBR.Symbols where

import Language.Drasil (QuantityDict, qw)
import Language.Drasil.Code (Mod(Mod), asVC, asVC')

import Drasil.GlassBR.IMods (gbrIMods)
import Drasil.GlassBR.ModuleDefs (allMods, implVars, interpY, interpZ)
import Drasil.GlassBR.Unitals (gbInputDataConstraints, gbInputs, gbOutputs,
    gBRSpecParamVals, glassBRSymbols, glassBRSymbolsWithDefns, glassBRUnitless)

import Data.List ((\\))

symbolsForTable :: [QuantityDict]
symbolsForTable = gbInputs ++ gbOutputs ++ (map qw gBRSpecParamVals) ++ 
  (map qw glassBRSymbolsWithDefns) ++ (map qw glassBRSymbols) ++
  (map qw glassBRUnitless) ++ (map qw gbInputDataConstraints) ++
  (map asVC' [interpY, interpZ]) 

this_symbols :: [QuantityDict]
this_symbols = (map qw gbrIMods) 
  -- include all module functions as symbols
  ++ ((map asVC $ concatMap (\(Mod _ l) -> l) allMods) \\ symbolsForTable)
  ++ map qw implVars ++ symbolsForTable
