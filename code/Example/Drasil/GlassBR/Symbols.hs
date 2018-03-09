module Drasil.GlassBR.Symbols where

import Language.Drasil
import Drasil.GlassBR.Unitals
import Drasil.GlassBR.ModuleDefs

this_symbols :: [QuantityDict]
this_symbols = map qw [prob_br] ++ gbInputs ++ (map qw gBRSpecParamVals) ++ 
  (map qw glassBRSymbolsWithDefns) ++ (map qw glassBRSymbols) ++
  (map qw glassBRUnitless) ++ (map qw gbInputDataConstraints)
  -- hack to include functions as symbols
  ++ map (qw . asVC) [interpY, interpZ, read_table, indInSeq, matrixCol, 
    linInterp, glassInputData]
  ++ (map qw [v_v, v_x_z_1, v_y_z_1, v_x_z_2, v_y_z_2, v_mat, v_col,
  v_i, v_j, v_k, v_z, v_z_array, v_y_array, v_x_array, v_y, v_arr, v_filename])
  ++ (map qw [v_y_2, v_y_1, v_x_2, v_x_1, v_x])
