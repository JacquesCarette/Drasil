module Drasil.GlassBR.Symbols where

import Language.Drasil
import Drasil.GlassBR.Unitals
import Drasil.GlassBR.ModuleDefs
import Data.Drasil.Utils (symbolMapFun)

this_symbols :: [QWrapper]
this_symbols = map qs [prob_br] ++ gbInputs ++ (map qs gBRSpecParamVals) ++ 
  (map qs glassBRSymbolsWithDefns) ++ (map qs glassBRSymbols) ++
  (map qs glassBRUnitless) ++ (map qs gbInputDataConstraints)
  -- hack to include functions as symbols
  ++ map (qs . asVC) [interpY, interpZ, read_table, indInSeq, matrixCol, 
    linInterp, glassInputData]
  ++ (map qs [v_v, v_x_z_1, v_y_z_1, v_x_z_2, v_y_z_2, v_mat, v_col,
  v_i, v_j, v_k, v_z, v_z_array, v_y_array, v_x_array, v_y, v_arr, v_filename])
  ++ (map qs [v_y_2, v_y_1, v_x_2, v_x_1, v_x])

{--}

gbSymbMapD :: QDefinition -> Contents
gbSymbMapD term_ = (symbolMapFun Data) term_

gbSymbMapT :: RelationConcept -> Contents
gbSymbMapT term_ = (symbolMapFun Theory) term_