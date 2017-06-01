module Modules.Interpolation (interpolation) where

import Language.Drasil.Code
import Prelude hiding (return)

-- TODO:  add gool support for non-object functions;  don't need a class for this
interpolation :: Module
interpolation = buildModule "Interpolation" [] [] [lin_interp_func, indInSeq_func] []

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

    
l_y1, l_y2, l_x1, l_x2, l_x, l_arr, l_v, l_i :: Label
l_x1 = "x1" 
l_y1 = "y1" 
l_x2 = "x2"
l_y2 = "y2" 
l_x = "x"
l_y = "y"
l_arr = "arr"
l_v = "v"
l_i = "i"

p_x1, p_y1, p_x2, p_y2, p_x, p_arr, p_v :: Parameter
p_x1 = param l_x1 float
p_y1 = param l_y1 float
p_x2 = param l_x2 float
p_y2 = param l_y2 float
p_x = param l_x float
p_arr = param l_arr (listT float)
p_v = param l_v float

v_x1, v_y1, v_x2, v_y2, v_x, v_y, v_arr, v_v, v_i :: Value
v_x1 = var l_x1
v_y1 = var l_y1
v_x2 = var l_x2
v_y2 = var l_y2
v_x = var l_x
v_y = var l_y
v_arr = var l_arr
v_v = var l_v
v_i = var l_i