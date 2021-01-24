module Symbols where

import Language.Drasil (QuantityDict, qw)
import Language.Drasil.Code (Mod(Mod), asVC, asVC')

import IMods (iMods)
import ModuleDefs (allMods, implVars, interpY, interpZ)
import Unitals (inputDataConstraints, inputs, outputs, 
  specParamVals, symbols, symbolsWithDefns, unitless, tmSymbols)

import Data.List ((\\))

symbolsForTable :: [QuantityDict]
symbolsForTable = inputs ++ outputs ++ tmSymbols ++ map qw specParamVals ++ 
  map qw symbolsWithDefns ++ map qw symbols ++ map qw unitless ++
  map qw inputDataConstraints ++ map asVC' [interpY, interpZ]

thisSymbols :: [QuantityDict]
thisSymbols = map qw iMods
  -- include all module functions as symbols
  ++ (map asVC (concatMap (\(Mod _ _ _ _ l) -> l) allMods) \\ symbolsForTable)
  ++ map qw implVars ++ symbolsForTable
