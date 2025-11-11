module Drasil.GlassBR.Symbols where

import Language.Drasil (DefinedQuantityDict, dqdWr, cnstrw', defLhs)
import Language.Drasil.Code (Mod(Mod), asVC)

import Drasil.GlassBR.ModuleDefs (allMods, implVars)
import Drasil.GlassBR.Assumptions (assumptionConstants)
import Drasil.GlassBR.Unitals (specParamVals, modElas,
  tmSymbols, interps, derivedInputDataConstraints, unitless, probBr,
  stressDistFac, nomThick, sdVector, inputsWUnitsUncrtn, inputsWUncrtn,
  glassTypeCon, unitalSymbols)

import Data.List ((\\))
import Control.Lens (view)

symbolsForSymbolTable :: [DefinedQuantityDict]
symbolsForSymbolTable = symbolsForTermTable ++ map dqdWr unitalSymbols ++
  unitless ++ map dqdWr [probBr, stressDistFac, cnstrw' nomThick, cnstrw' glassTypeCon] ++
  map dqdWr derivedInputDataConstraints

symbolsForTermTable :: [DefinedQuantityDict]
symbolsForTermTable = map dqdWr inputsWUnitsUncrtn ++ map dqdWr inputsWUncrtn ++
  map dqdWr sdVector ++ tmSymbols ++ map dqdWr specParamVals ++ 
  map (view defLhs) assumptionConstants ++ [dqdWr modElas] ++ interps

  -- include all module functions as symbols
thisSymbols :: [DefinedQuantityDict]
thisSymbols = (map asVC (concatMap (\(Mod _ _ _ _ l) -> l) allMods)
  \\ symbolsForSymbolTable) ++ implVars ++ symbolsForSymbolTable

thisTerms :: [DefinedQuantityDict]
thisTerms = (map asVC (concatMap (\(Mod _ _ _ _ l) -> l) allMods)
  \\ symbolsForTermTable) ++ implVars ++ symbolsForTermTable
