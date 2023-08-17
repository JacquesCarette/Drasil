module Drasil.GlassBR.Symbols where

import Control.Lens ((^.))

import Language.Drasil (QuantityDict, qw)
import Language.Drasil.Code (Mod(Mod), asVC)
import Theory.Drasil (output)

import Drasil.GlassBR.IMods (iMods)
import Drasil.GlassBR.ModuleDefs (allMods, implVars)
import Drasil.GlassBR.Unitals (inputDataConstraints, inputs, outputs, 
  specParamVals, symbols, symbolsWithDefns, unitless, tmSymbols, interps)

import Data.List ((\\))

symbolsForTable :: [QuantityDict]
symbolsForTable = inputs ++ outputs ++ tmSymbols ++ map qw specParamVals ++ 
  map qw symbolsWithDefns ++ map qw symbols ++ map qw unitless ++
  map qw inputDataConstraints ++ interps

thisSymbols :: [QuantityDict]
thisSymbols = map (^. output) iMods
  -- include all module functions as symbols
  ++ (map asVC (concatMap (\(Mod _ _ _ _ l) -> l) allMods) \\ symbolsForTable)
  ++ map qw implVars ++ symbolsForTable
