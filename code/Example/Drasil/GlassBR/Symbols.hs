module Drasil.GlassBR.Symbols where

import Language.Drasil
import Drasil.GlassBR.Unitals
import Drasil.GlassBR.ModuleDefs (implVars,allMods)

this_symbols :: [QuantityDict]
this_symbols = map qw [prob_br] ++ gbInputs ++ (map qw gBRSpecParamVals) ++ 
  (map qw glassBRSymbolsWithDefns) ++ (map qw glassBRSymbols) ++
  (map qw glassBRUnitless) ++ (map qw gbInputDataConstraints)
  -- hack to include functions as symbols
  ++ (map (qw . asVC) $ concatMap (\(Mod _ l) -> l) allMods)
  ++ map qw implVars
