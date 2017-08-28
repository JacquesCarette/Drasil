module Drasil.GlassBR.Symbols where

import Language.Drasil
import Drasil.GlassBR.Unitals
import Drasil.GlassBR.ModuleDefs
import Data.Drasil.Utils (symbolMapFun)

this_symbols :: [QWrapper]
this_symbols = (map qs [prob_br] ++ gbInputs ++ (map qs gBRSpecParamVals) ++ 
  (map qs glassBRSymbolsWithDefns) ++ (map qs glassBRSymbols) ++
  (map qs glassBRUnitless)
  -- hack to include functions as symbols
  ++ map (qs . asVC) [interpY, interpZ])

{--}

gbSymbMap :: SymbolMap
gbSymbMap = symbolMap this_symbols

gbSymbMapD :: QDefinition -> Contents
gbSymbMapD term_ = (symbolMapFun gbSymbMap Data) term_

gbSymbMapT :: RelationConcept -> Contents
gbSymbMapT term_ = (symbolMapFun gbSymbMap Theory) term_