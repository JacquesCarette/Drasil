module Drasil.GlassBR.Symbols where

import Language.Drasil (QuantityDict, qw)
import Language.Drasil.Code (Mod(Mod), asVC)

import Drasil.GlassBR.DataDefs (r6DDs)
import Drasil.GlassBR.IMods (iMods)
import Drasil.GlassBR.ModuleDefs (allMods, implVars)
import Drasil.GlassBR.Unitals (inputDataConstraints, inputs,
  specParamVals, symbols, symbolsWithDefns, unitless, tmSymbols, interps, probBr)

import Data.List ((\\))

symbolsForTable :: [QuantityDict]
symbolsForTable = inputs ++ map qw iMods ++ map qw r6DDs ++ tmSymbols ++ map qw specParamVals ++ 
  map qw symbolsWithDefns ++ map qw symbols ++ map qw unitless ++
  map qw inputDataConstraints ++ interps

thisSymbols :: [QuantityDict]
thisSymbols = qw probBr : map qw iMods ++ map qw r6DDs
  -- include all module functions as symbols
  ++ (map asVC (concatMap (\(Mod _ _ _ _ l) -> l) allMods) \\ symbolsForTable)
  ++ map qw implVars ++ symbolsForTable
