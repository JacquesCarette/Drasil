module Modules.OutputFormat (outputFormat) where

import Language.Drasil.Code
import Defs

outputFormat :: Module
outputFormat = buildModule "OutputFormat" [lib_InputParameters] [] [display_output_func] []

display_output_func :: FunctionDecl
display_output_func = pubMethod methodTypeVoid "display_output" 
  [ p_filename, 
    p_q,
    p_j,
    p_q_hat_tol,
    p_pb,
    p_lr,
    p_nfl,
    p_is_safe1,
    p_is_safe2,
    p_params
  ]
    [ 
      block (
           [ varDec l_outfile outfile ,
             openFileW v_outfile v_filename ]
        ++ concatMap (\x -> printField x float) param_float_fields
        ++ concatMap (\x -> printField x int) param_int_fields
        ++ concatMap (\x -> printOutput x float) float_outputs
        ++ concatMap (\x -> printOutput x bool) bool_outputs
        ++ [ ifCond 
               [ ( 
                   v_is_safe1 ?&& v_is_safe2 ,
                   (oneLiner $ printFileStrLn v_outfile 
                     "For the given input parameters, the glass is considered safe.")
                 )
               ]
               (oneLiner $ printFileStrLn v_outfile
                 "For the given input parameters, the glass is NOT considered safe.") ,
             closeFile v_outfile ]
      )
    ]    
   

--helper
printField :: String -> StateType -> [Statement]
printField field s = [
    printFileStr v_outfile $ addSpace field 12 ,
    printFileLn v_outfile s $ v_params$->(var field)
  ]
  
printOutput :: (String, Label) -> StateType -> [Statement]
printOutput (desc, l) s = [
    printFileStr v_outfile $ addSpace (desc ++ " (" ++ l ++ ")") 32 ,
    printFileLn v_outfile s $ var l
  ]
  
addSpace :: String -> Int -> String
addSpace s i = s ++ replicate (i - length s) ' '    