module Modules.Control (control) where

import Language.Drasil.Code
import Defs
import Prelude hiding (return)

control :: Module
control = buildModule "Control" 
  [ lib_InputParameters, 
    lib_InputFormat, 
    lib_DerivedValues, 
    lib_InputConstraints, 
    lib_Interpolation, 
    lib_Calculations, 
    lib_OutputFormat,
    "readTable"
  ] [] [main_func] []

main_func :: FunctionDecl
main_func = mainMethod 
  [
    block [
      varDecDef l_filename string $ arg 0 ,
      objDecNewVoid l_params lib_InputParameters s_InputParameters ,
      valStmt $ funcApp lib_InputFormat "get_input" [v_filename, v_params] ,     
      valStmt $ funcApp lib_DerivedValues "derived_params" [v_params] ,      
      valStmt $ funcApp lib_InputConstraints "check_constraints" [v_params] ,
      -- TODO:  this is probably wrong, will need specialized handling for list declarations w/ assignment:
      varDecDef l_w_array (listT float) $ funcApp "readTable" "read_z_array" [litString "TSD.txt"] ,
      varDecDef l_data_sd (listT $ listT float) $ funcApp "readTable" "read_x_array" [litString "TSD.txt", v_w_array$.listSize] ,
      varDecDef l_data_q (listT $ listT float) $ funcApp "readTable" "read_y_array" [litString "TSD.txt", v_w_array$.listSize] ,
      varDecDef l_j_array (listT float) $ funcApp "readTable" "read_z_array" [litString "SDF.txt"] ,
      varDecDef l_data_asprat (listT $ listT float) $ funcApp "readTable" "read_x_array" [litString "SDF.txt", v_j_array$.listSize] ,
      varDecDef l_data_qstar (listT $ listT float) $ funcApp "readTable" "read_y_array" [litString "SDF.txt", v_j_array$.listSize] ,
      varDecDef l_q float $ funcApp lib_Interpolation "interpY" [v_data_sd, v_data_q, v_w_array, v_params$->v_sd, v_params$->v_wtnt] ,
      varDecDef l_q_hat float $ funcApp lib_Calculations "calc_q_hat" [v_q, v_params] ,
      varDecDef l_j_tol float $ funcApp lib_Calculations "calc_j_tol" [v_params] ,
      varDecDef l_j float $ funcApp lib_Interpolation "interpZ" [v_data_asprat, v_data_qstar, v_j_array, v_params$->v_asprat, v_q_hat] ,
      varDecDef l_q_hat_tol float $ funcApp lib_Interpolation "interpY" [v_data_asprat, v_data_qstar, v_j_array, v_params$->v_asprat, v_j_tol] ,
      varDecDef l_pb float $ funcApp lib_Calculations "calc_pb" [v_j, v_params] ,
      varDecDef l_nfl float $ funcApp lib_Calculations "calc_nfl" [v_q_hat_tol, v_params] ,
      varDecDef l_lr float $ funcApp lib_Calculations "calc_lr" [v_nfl, v_params] ,
      varDecDef l_is_safe1 bool $ funcApp lib_Calculations "calc_is_safe1" [v_pb, v_params] ,
      varDecDef l_is_safe2 bool $ funcApp lib_Calculations "calc_is_safe2" [v_lr, v_q] ,
      valStmt $ funcApp lib_OutputFormat "display_output" [litString "outputfile.txt", v_q, v_j, v_q_hat_tol, v_pb, v_lr, v_nfl, v_is_safe1, v_is_safe2, v_params] ,
      printStrLn "Main has been executed and the results have been written to 'outputfile.txt'."
    ]
  ]