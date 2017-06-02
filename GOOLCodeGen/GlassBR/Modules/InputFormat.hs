module Modules.InputFormat (inputFormat) where

import Language.Drasil.Code
import Defs

inputFormat :: Module
inputFormat = buildModule "InputFormat" [] [] [get_input_func] []

get_input_func :: FunctionDecl
get_input_func = pubMethod methodTypeVoid "get_input" [p_filename, p_params] 
    [ 
      block [
        varDec l_infile infile,
        openFileR v_infile v_filename,
        getFileInput v_infile float $ v_params$->v_a,
        getFileInput v_infile float $ v_params$->v_b,
        getFileInput v_infile float $ v_params$->v_t,
        getFileInput v_infile int $ v_params$->v_gt,
        getFileInput v_infile float $ v_params$->v_w,
        getFileInput v_infile float $ v_params$->v_tnt,
        getFileInput v_infile float $ v_params$->v_sdx,
        getFileInput v_infile float $ v_params$->v_sdy,
        getFileInput v_infile float $ v_params$->v_sdz,
        getFileInput v_infile float $ v_params$->v_pbtol,
        closeFile v_infile
      ]
    ]      
