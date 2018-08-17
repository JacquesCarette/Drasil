module Drasil.GlassBR.Symbols where

import Language.Drasil
import Language.Drasil.Code (Mod(Mod), asVC)

import Drasil.GlassBR.IMods (gbrIMods)
import Drasil.GlassBR.ModuleDefs (allMods, implVars)
import Drasil.GlassBR.Unitals (gbInputDataConstraints, gbInputs, 
    gBRSpecParamVals, glassBRSymbols, glassBRSymbolsWithDefns, glassBRUnitless)

symbolsForTable :: [QuantityDict]
symbolsForTable = gbInputs ++ (map qw gBRSpecParamVals) ++ 
  (map qw glassBRSymbolsWithDefns) ++ (map qw glassBRSymbols) ++
  (map qw glassBRUnitless) ++ (map qw gbInputDataConstraints)
  -- include all module functions as symbols
  ++ (map (qw . asVC) $ concatMap (\(Mod _ l) -> l) allMods)
  ++ map qw implVars

this_symbols :: [QuantityDict]
this_symbols = (map qw gbrIMods) ++ gbInputs ++ (map qw gBRSpecParamVals) ++ 
  (map qw glassBRSymbolsWithDefns) ++ (map qw glassBRSymbols) ++
  (map qw glassBRUnitless) ++ (map qw gbInputDataConstraints)
  -- include all module functions as symbols
  ++ (map (qw . asVC) $ concatMap (\(Mod _ l) -> l) allMods)
  ++ map qw implVars
