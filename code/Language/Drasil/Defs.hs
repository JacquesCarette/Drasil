-- THIS MODULE IS A HACK FOR CODE GEN, TO BE DELETED ASAP --
module Language.Drasil.Defs where

import Language.Drasil.Code
import Prelude hiding (return)

-- interpolation functions
lin_interp_func :: FunctionDecl
lin_interp_func = pubMethod (methodType float) "lin_interp" [p_x1, p_y1, p_x2, p_y2, p_x] 
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
        listDec' l_col float 0,
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
            v_y2,
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

-- read table functions
read_z_array_func :: FunctionDecl
read_z_array_func = pubMethod (methodType $ listT float)  "read_z_array" [p_filename] 
    [ 
      block [
        varDec l_infile infile,
        openFileR v_infile v_filename,
        varDec l_line string,
        getFileInputLine v_infile v_line,
        closeFile v_infile,
        listDec' l_z_array_str string 0,
        stringSplit v_z_array_str v_line ',',
        listSlice string v_z_array_str v_z_array_str (Just $ litInt 1) Nothing (Just $ litInt 2),
        listDec' l_z_array float 0,
        for (varDecDef l_i int (litInt 0)) (v_i ?< v_z_array_str$.listSize) ((&++) v_i) 
          (oneLiner $ valStmt $ v_z_array$.(listAppend $ (v_z_array_str$.(listAccess v_i)$.(cast float string)))),
        return v_z_array
      ]
    ]     

read_x_array_func :: FunctionDecl
read_x_array_func = pubMethod (methodType $ listT $ listT float) "read_x_array" [p_filename] 
    [ 
      block [
        varDec l_infile infile,
        openFileR v_infile v_filename,
        listDec' l_lines string 0,
        getFileInputAll v_infile v_lines,
        closeFile v_infile,
        listSlice string v_lines v_lines (Just $ litInt 1) Nothing Nothing,
        listDec' l_x_array_str (listT string) 0,
        for (varDecDef l_i int (litInt 0)) (v_i ?< v_lines$.listSize) ((&++) v_i)
          [
            block [
              listDec' "temp_str" string 0,
              stringSplit (var "temp_str") (v_lines$.(listAccess v_i)) ',',
              listSlice string (var "temp_str") (var "temp_str") (Just $ litInt 0) Nothing (Just $ litInt 2),
              valStmt $ v_x_array_str$.(listAppend (var "temp_str"))
            ]
          ],
        listDec' l_x_array (listT float) 0,
        for (varDecDef l_i int (litInt 0)) (v_i ?< v_x_array_str$.listSize) ((&++) v_i)
          [
            block [
              listDec' l_nextLine float 0,
              for (varDecDef l_j int (litInt 0)) (v_j ?< v_x_array_str$.(listAccess v_i)$.listSize) ((&++) v_j)
                (oneLiner $ valStmt$ v_nextLine$.(listAppend $ v_x_array_str$.(listAccess v_i)$.(listAccess v_j)$.(cast float string))),
              valStmt $ v_x_array$.(listAppend v_nextLine)                
            ]
          ],
        return v_x_array
      ]
    ]     

read_y_array_func :: FunctionDecl
read_y_array_func = pubMethod (methodType $ listT $ listT float) "read_y_array" [p_filename] 
    [ 
      block [
        varDec l_infile infile,
        openFileR v_infile v_filename,
        listDec' l_lines string 0,
        getFileInputAll v_infile v_lines,
        closeFile v_infile,
        listSlice string v_lines v_lines (Just $ litInt 1) Nothing Nothing,
        listDec' l_y_array_str (listT string) 0,
        for (varDecDef l_i int (litInt 0)) (v_i ?< v_lines$.listSize) ((&++) v_i)
          [
            block [
              listDec' "temp_str" string 0,
              stringSplit (var "temp_str") (v_lines$.(listAccess v_i)) ',',
              listSlice string (var "temp_str") (var "temp_str") (Just $ litInt 1) Nothing (Just $ litInt 2),
              valStmt $ v_y_array_str$.(listAppend (var "temp_str"))
            ]
          ],
        listDec' l_y_array (listT float) 0,
        for (varDecDef l_i int (litInt 0)) (v_i ?< v_y_array_str$.listSize) ((&++) v_i)
          [
            block [
              listDec' l_nextLine float 0,
              for (varDecDef l_j int (litInt 0)) (v_j ?< v_y_array_str$.(listAccess v_i)$.listSize) ((&++) v_j)
                (oneLiner $ valStmt$ v_nextLine$.(listAppend $ v_y_array_str$.(listAccess v_i)$.(listAccess v_j)$.(cast float string))),
              valStmt $ v_y_array$.(listAppend v_nextLine)                
            ]
          ],
        return v_y_array
      ]
    ]  

   

-- gool labels/variables/params

l_filename, l_infile, l_a, l_b, l_t, l_gt, l_w, l_tnt, l_sdx, l_sdy,
  l_sdz, l_pbtol, l_asprat, l_sd, l_h, l_gtf, l_ldf, l_wtnt, l_E,
  l_td, l_m, l_k, l_lsf, l_w_array, l_data_sd, l_data_q, l_j_array, 
  l_data_asprat, l_data_qstar, l_params, l_q, l_j_tol, l_q_hat, l_q_hat_tol,
  l_pb, l_j, l_nfl, l_lr, l_is_safe1, l_is_safe2, l_y1, l_y2, l_x1, l_x2, 
  l_x, l_y, l_arr, l_v, l_i, l_mat, l_c, l_col, l_x_array, l_y_array, 
  l_z_array, l_z, l_x_z1, l_y_z1, l_x_z2, l_y_z2, l_y_upper, l_y_lower,
  l_outfile, l_line, l_lines, l_x_array_str, l_y_array_str, l_nextLine, 
  l_z_array_str  :: Label
l_filename = "filename"
l_infile = "infile"
l_a = "a"
l_b = "b"
l_t = "t"
l_gt = "gt"
l_w = "w"
l_tnt = "tnt"
l_sdx = "sdx"
l_sdy = "sdy"
l_sdz = "sdz"
l_pbtol = "pbtol"
l_asprat = "asprat"
l_sd = "sd"
l_h = "h"
l_gtf = "gtf"
l_ldf = "ldf"
l_wtnt = "wtnt"
l_E = "E"
l_td = "td"
l_m = "m"
l_k = "k"
l_lsf = "lsf"
l_w_array = "w_array"
l_data_sd = "data_sd"
l_data_q = "data_q"
l_j_array = "j_array"
l_data_asprat = "data_asprat"
l_data_qstar = "data_qstar"
l_params = "inparams"   
l_q = "q"
l_j_tol = "j_tol"
l_q_hat = "q_hat"
l_q_hat_tol = "q_hat_tol"
l_pb = "pb"
l_j = "j"
l_nfl = "nfl"
l_lr = "lr"
l_is_safe1 = "is_safe1"
l_is_safe2 = "is_safe2"
l_x1 = "x1" 
l_y1 = "y1" 
l_x2 = "x2"
l_y2 = "y2" 
l_x = "x"
l_y = "y"
l_arr = "arr"
l_v = "v"
l_i = "i"
l_mat = "mat"
l_c = "c"
l_col = "col"
l_x_array = "x_array"
l_y_array = "y_array"
l_z_array = "z_array"
l_z = "z"
l_x_z1 = "x_z1"
l_y_z1 = "y_z1"
l_x_z2 = "x_z2"
l_y_z2 = "y_z2"
l_y_upper = "y_upper"
l_y_lower = "y_lower"
l_outfile = "outfile"
l_line = "line"
l_lines = "lines"
l_x_array_str = "x_array_str"
l_y_array_str = "y_array_str"
l_nextLine = "nextLine"
l_z_array_str = "z_array_str"


v_filename, v_infile, v_a, v_b, v_t, v_gt, v_w, v_tnt, v_sdx, v_sdy,
  v_sdz, v_pbtol, v_asprat, v_sd, v_h, v_gtf, v_ldf, v_wtnt,
  v_E, v_td, v_m, v_k, v_lsf, v_w_array, v_data_sd, v_data_q, 
  v_j_array, v_data_asprat, v_data_qstar, v_params, v_q, v_j_tol, 
  v_q_hat, v_q_hat_tol, v_pb, v_j, v_nfl, v_lr, v_is_safe1, 
  v_is_safe2, v_x1, v_y1, v_x2, v_y2, v_x, v_y, v_arr, v_v, v_i, 
  v_mat, v_c, v_col, v_x_array, v_y_array, v_z_array, v_z, v_x_z1, 
  v_y_z1, v_x_z2, v_y_z2, v_y_upper, v_y_lower, v_outfile, v_line, 
  v_lines, v_x_array_str, v_y_array_str, v_nextLine, v_z_array_str  :: Value
v_filename = var l_filename
v_infile = var l_infile
v_a = var l_a
v_b = var l_b
v_t = var l_t
v_gt = var l_gt
v_w = var l_w
v_tnt = var l_tnt
v_sdx = var l_sdx
v_sdy = var l_sdy
v_sdz = var l_sdz
v_pbtol = var l_pbtol
v_asprat = var l_asprat
v_sd = var l_sd
v_h = var l_h
v_gtf = var l_gtf
v_ldf = var l_ldf
v_wtnt = var l_wtnt
v_E = var l_E
v_td = var l_td
v_m = var l_m
v_k = var l_k
v_lsf = var l_lsf
v_w_array = var l_w_array
v_data_sd = var l_data_sd
v_data_q = var l_data_q
v_j_array = var l_j_array
v_data_asprat = var l_data_asprat
v_data_qstar = var l_data_qstar
v_params = var l_params
v_q = var l_q
v_j_tol = var l_j_tol
v_q_hat = var l_q_hat
v_q_hat_tol = var l_q_hat_tol
v_pb = var l_pb
v_j = var l_j
v_nfl = var l_nfl
v_lr = var l_lr
v_is_safe1 = var l_is_safe1
v_is_safe2 = var l_is_safe2
v_x1 = var l_x1
v_y1 = var l_y1
v_x2 = var l_x2
v_y2 = var l_y2
v_x = var l_x
v_y = var l_y
v_arr = var l_arr
v_v = var l_v
v_i = var l_i
v_mat = var l_mat
v_c = var l_c
v_col = var l_col
v_x_array = var l_x_array
v_y_array = var l_y_array
v_z_array = var l_z_array
v_z = var l_z
v_x_z1 = var l_x_z1
v_y_z1 = var l_y_z1
v_x_z2 = var l_x_z2
v_y_z2 = var l_y_z2
v_y_upper = var l_y_upper
v_y_lower = var l_y_lower
v_outfile = var l_outfile
v_line = var l_line
v_lines = var l_lines
v_x_array_str = var l_x_array_str
v_y_array_str = var l_y_array_str
v_nextLine = var l_nextLine
v_z_array_str = var l_z_array_str


p_filename, p_w_array, p_data_sd, p_data_q, p_params, p_q, p_j, p_q_hat_tol, 
  p_nfl, p_pb, p_lr, p_x1, p_y1, p_x2, p_y2, p_x, p_arr, p_v, p_mat, 
  p_c, p_x_array, p_y_array, p_z_array, p_z, p_y, p_is_safe1, p_is_safe2 :: Parameter
p_filename = param l_filename string
p_w_array = param l_w_array (listT float)
p_data_sd = param l_data_sd (listT $ listT float)
p_data_q = param l_data_q (listT $ listT float)
p_params = param l_params (obj "InputParameters")
p_q = param l_q float
p_j = param l_j float
p_q_hat_tol = param l_q_hat_tol float
p_nfl = param l_nfl float
p_pb = param l_pb float
p_lr = param l_lr float
p_x1 = param l_x1 float
p_y1 = param l_y1 float
p_x2 = param l_x2 float
p_y2 = param l_y2 float
p_x = param l_x float
p_arr = param l_arr (listT float)
p_v = param l_v float
p_mat = param l_mat (listT $ listT float)
p_c = param l_c int
p_x_array = param l_x_array (listT $ listT float)
p_y_array = param l_y_array (listT $ listT float)
p_z_array = param l_z_array (listT float)
p_z = param l_z float
p_y = param l_y float
p_is_safe1 = param l_is_safe1 bool
p_is_safe2 = param l_is_safe2 bool


s_InputParameters :: StateType
s_InputParameters = obj "InputParameters"

lib_InputParameters, lib_InputFormat, lib_DerivedValues, lib_InputConstraints,
  lib_Interpolation, lib_Calculations, lib_OutputFormat, lib_ReadTable :: Library
lib_InputParameters = "InputParameters"
lib_InputFormat = "InputFormat"
lib_DerivedValues = "DerivedValues"
lib_InputConstraints = "InputConstraints"
lib_Interpolation = "Interpolation"
lib_Calculations = "Calculations"
lib_OutputFormat = "OutputFormat"
lib_ReadTable = "ReadTable"


param_float_fields :: [String]
param_float_fields = [
      l_a ,
      l_b ,
      l_t ,
      l_w ,
      l_tnt ,
      l_sdx ,
      l_sdy ,
      l_sdz ,
      l_pbtol ,
      l_asprat ,
      l_sd ,
      l_h ,
      l_gtf ,
      l_ldf ,
      l_wtnt ,
      l_E ,
      l_td ,
      l_m ,
      l_k ,
      l_lsf
    ]

param_int_fields :: [String]
param_int_fields = [l_gt]   
    
float_outputs :: [(String, Label)]
float_outputs = [
    ("Demand", l_q) ,
    ("Stress Distr. Factor", l_j) ,
    ("Tolerable Pressure", l_q_hat_tol) ,
    ("Prob. of Breakage", l_pb) ,
    ("Capacity", l_lr) ,
    ("Non-Factored Load", l_nfl)
  ]
  
bool_outputs :: [(String, Label)]
bool_outputs = [
    ("Safety Req. 1", l_is_safe1) ,
    ("Safety Req. 2", l_is_safe2)
  ]
