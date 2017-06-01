module Modules.Interpolation (interpolation) where

import Language.Drasil.Code
import Defs
import Prelude hiding (return)

-- TODO:  add gool support for non-object functions;  don't need a class for this
interpolation :: Module
interpolation = buildModule "Interpolation" [] [] [lin_interp_func, indInSeq_func, matrixCol_func, interpY_func, interpZ_func] []

lin_interp_func :: FunctionDecl
lin_interp_func = pubMethod (methodType float) "lin_interp" [p_y1, p_y2, p_x1, p_x2, p_x] 
    [ 
      block [
        varDecDef l_y float $ (v_y2 #- v_y1) #/ (v_x2 #- v_x1) #* (v_x #- v_x1) #+ v_y1,
        return $ v_y
      ]
    ]     

indInSeq_func :: FunctionDecl
indInSeq_func = pubMethod (methodType int) "indInSeq" [p_arr, p_v] 
    [ 
      block [
        for (varDecDef l_i int (litInt 0)) (v_i ?< v_arr$.listSize #- (litInt 1)) ((&++) v_i) 
          (oneLiner (ifCond 
            [(v_arr$.(listAccess v_i) ?<= v_v ?&& v_v ?<= v_arr$.(listAccess (v_i #+ (litInt 1))), oneLiner $ return v_i)] 
            noElse
            )
          ) ,
        throw "Index not found"
      ]
    ]     
    
matrixCol_func :: FunctionDecl
matrixCol_func = pubMethod (methodType $ listT float) "matrixCol" [p_mat, p_c] 
    [ 
      block [
        listDec' l_col float 100,
        for (varDecDef l_i int (litInt 0)) (v_i ?< v_mat$.listSize) ((&++) v_i) 
          (oneLiner (valStmt $ v_col$.(listAppend (v_mat$.(listAccess v_i)$.(listAccess v_c))))) ,
        return v_col
      ]
    ] 
    
interpY_func :: FunctionDecl
interpY_func = pubMethod (methodType float) "interpY" [p_x_array, p_y_array, p_z_array, p_x, p_z] 
    [ 
      block [
        varDecDef l_i int $ funcApp' "indInSeq" [v_z_array, v_z] ,
        -- TODO:  refine gool list mechanics (varDecDef is wrong here)
        varDecDef l_x_z1 (listT float) $ funcApp' "matrixCol" [v_x_array, v_i] ,
        varDecDef l_y_z1 (listT float) $ funcApp' "matrixCol" [v_y_array, v_i] ,
        varDecDef l_x_z2 (listT float) $ funcApp' "matrixCol" [v_x_array, v_i #+ (litInt 1)] ,
        varDecDef l_y_z2 (listT float) $ funcApp' "matrixCol" [v_y_array, v_i #+ (litInt 1)] ,
        varDec l_j int ,
        varDec l_k int ,
        tryCatch 
          [ 
            block [
              v_j &= (funcApp' "indInSeq" [v_x_z1, v_x]) ,
              v_k &= (funcApp' "indInSeq" [v_x_z2, v_x])
            ]
          ] 
          ( oneLiner $ throw "Interpolation of y failed." ) ,
        varDecDef l_y1 float $ funcApp' "lin_interp" 
          [ v_x_z1$.(listAccess v_j), 
            v_y_z1$.(listAccess v_j), 
            v_x_z1$.(listAccess (v_j #+ (litInt 1))),
            v_y_z1$.(listAccess (v_j #+ (litInt 1))),
            v_x ] ,
        varDecDef l_y2 float $ funcApp' "lin_interp" 
          [ v_x_z2$.(listAccess v_k), 
            v_y_z2$.(listAccess v_k), 
            v_x_z2$.(listAccess (v_k #+ (litInt 1))),
            v_y_z2$.(listAccess (v_k #+ (litInt 1))),
            v_x ] ,
        return $ funcApp' "lin_interp" 
          [ v_z_array$.(listAccess v_i),
            v_y1,
            v_z_array$.(listAccess (v_i #+ (litInt 1))),
            v_y1,
            v_z ]
      ]
    ]

    
interpZ_func :: FunctionDecl
interpZ_func = pubMethod (methodType float) "interpZ" [p_x_array, p_y_array, p_z_array, p_x, p_y] 
    [ 
      block [
        for (varDecDef l_i int (litInt 0)) (v_i ?< v_z_array$.listSize #- (litInt 1)) ((&++) v_i) 
          [
            block [
              varDecDef l_i int $ funcApp' "indInSeq" [v_z_array, v_z] ,
              -- TODO:  refine gool list mechanics (varDecDef is wrong here)
              varDecDef l_x_z1 (listT float) $ funcApp' "matrixCol" [v_x_array, v_i] ,
              varDecDef l_y_z1 (listT float) $ funcApp' "matrixCol" [v_y_array, v_i] ,
              varDecDef l_x_z2 (listT float) $ funcApp' "matrixCol" [v_x_array, v_i #+ (litInt 1)] ,
              varDecDef l_y_z2 (listT float) $ funcApp' "matrixCol" [v_y_array, v_i #+ (litInt 1)] ,
              varDec l_j int ,
              varDec l_k int ,
              tryCatch 
                [ 
                  block [
                    v_j &= (funcApp' "indInSeq" [v_x_z1, v_x]) ,
                    v_k &= (funcApp' "indInSeq" [v_x_z2, v_x])
                  ]
                ] 
                ( oneLiner $ continue ) ,
              varDecDef l_y_lower float $ funcApp' "lin_interp" 
                [ v_x_z1$.(listAccess v_j), 
                  v_y_z1$.(listAccess v_j), 
                  v_x_z1$.(listAccess (v_j #+ (litInt 1))),
                  v_y_z1$.(listAccess (v_j #+ (litInt 1))),
                  v_x ] ,
              varDecDef l_y_upper float $ funcApp' "lin_interp" 
                [ v_x_z2$.(listAccess v_k), 
                  v_y_z2$.(listAccess v_k), 
                  v_x_z2$.(listAccess (v_k #+ (litInt 1))),
                  v_y_z2$.(listAccess (v_k #+ (litInt 1))),
                  v_x ] ,
              ifCond 
                [ ( v_y_lower ?<= v_y ?&& v_y ?<= v_y_upper, 
                    oneLiner $ return $ funcApp' "lin_interp" 
                      [ v_y_lower,
                        v_z_array$.(listAccess v_i),
                        v_y_upper,
                        v_z_array$.(listAccess (v_i #+ (litInt 1))),
                        v_y ]
                  )
                ] noElse
            ]
          ] ,
        throw "Interpolation of z failed."
      ]
    ]    
        