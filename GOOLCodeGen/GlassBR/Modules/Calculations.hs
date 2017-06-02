module Modules.Calculations (calculations) where

import Language.Drasil.Code
import Defs
import Prelude hiding (return, log, exp)

calculations :: Module
calculations = buildModule "Calculations" [] [] 
  [calc_q_hat_func, calc_j_tol_func, calc_pb_func, calc_nfl_func, calc_lr_func, calc_is_safe1_func, calc_is_safe2_func] []

calc_q_hat_func :: FunctionDecl
calc_q_hat_func = pubMethod (methodType float) "calc_q_hat" [p_q, p_params] 
    [ 
      block [
        varDecDef l_q_hat float $ v_q #* (v_params$->v_a #* v_params$->v_b) #^ (litFloat 2.0) #/ (v_params$->v_E #* v_params$->v_h #^ (litFloat 4.0)) #* ((litFloat 1.0) #/ v_params$->v_gtf) ,
        return v_q_hat
      ]
    ]     
    
calc_j_tol_func :: FunctionDecl
calc_j_tol_func = pubMethod (methodType float) "calc_j_tol" [p_params]
    [
      block [
        varDecDef l_j_tol float $ log((log((litFloat 1.0) #/ ((litFloat 1.0) #- v_params$->v_pbtol))) #* ((((v_params$->v_a #/ (litFloat 1000.0)) #* (v_params$->v_b #/ (litFloat 1000.0))) #^ (v_params$->v_m #- (litFloat 1.0))) #/ ((v_params$->v_k #* ((v_params$->v_E #* (litFloat 1000.0) #* (v_params$->v_h #/ (litFloat 1000.0)) #^ (litFloat 2.0)) #^ v_params$->v_m)) #* v_params$->v_ldf))) ,
        return v_j_tol
      ]
    ]

calc_pb_func :: FunctionDecl
calc_pb_func = pubMethod (methodType float) "calc_pb" [p_j, p_params]
    [
      block [
        varDecDef l_b float $ (v_params$->v_k #/ ((v_params$->v_a #/ (litFloat 1000.0) #* v_params$->v_b #/ (litFloat 1000.0)) #^ (v_params$->v_m #- (litFloat 1.0)))) #* (((litFloat 1000.0) #* v_params$->v_E #* (v_params$->v_h #/ (litFloat 1000.0)) #^ (litFloat 2.0)) #^ v_params$->v_m) #* v_params$->v_ldf #* (exp v_j) ,
        varDecDef l_pb float $ (litFloat 1.0) #- (exp $ (#~) v_b) ,
        return v_pb
      ]
    ]   
    
calc_nfl_func :: FunctionDecl
calc_nfl_func = pubMethod (methodType float) "calc_nfl" [p_q_hat_tol, p_params]
    [
      block [
        varDecDef l_nfl float $ (v_q_hat_tol #* v_params$->v_E #* (v_params$->v_h) #^ (litFloat 4.0)) #/ ((v_params$->v_a #* v_params$->v_b) #^ (litFloat 2.0)) ,
        return v_nfl
      ]
    ]  
    
calc_lr_func :: FunctionDecl
calc_lr_func = pubMethod (methodType float) "calc_lr" [p_nfl, p_params]
    [
      block [
        varDecDef l_lr float $ v_nfl #* v_params$->v_gtf #* v_params$->v_lsf ,
        return v_lr
      ]
    ]    

calc_is_safe1_func :: FunctionDecl
calc_is_safe1_func = pubMethod (methodType bool) "calc_is_safe1" [p_pb, p_params]
    [
      block [
        varDec l_is_safe1 bool ,
        ifCond [(v_pb ?< v_params$->v_pbtol, oneLiner (v_is_safe1 &= true))] (oneLiner (v_is_safe1 &= false)) ,
        return v_is_safe1
      ]
    ]    

calc_is_safe2_func :: FunctionDecl
calc_is_safe2_func = pubMethod (methodType bool) "calc_is_safe2" [p_lr, p_q]
    [
      block [
        varDec l_is_safe2 bool ,
        ifCond [(v_lr ?> v_q, oneLiner (v_is_safe2 &= true))] (oneLiner (v_is_safe2 &= false)) ,
        return v_is_safe2
      ]
    ]