module Modules.InputFormat (inputFormat) where

import Language.Drasil.Code

inputFormat :: Module
inputFormat = buildModule "InputFormat" [] [] [inputFormatFunc] []

inputFormatFunc :: FunctionDecl
inputFormatFunc = pubMethod methodTypeVoid "get_input" [p_filename, p_params] 
    [ 
      block [
        varDec "infile" infile,
        openFileR v_infile v_filename,
        getFileInput v_infile float $ v_params$->(var "a"),
        getFileInput v_infile float $ v_params$->(var "b"),
        getFileInput v_infile float $ v_params$->(var "t"),
        getFileInput v_infile int $ v_params$->(var "gt"),
        getFileInput v_infile float $ v_params$->(var "w"),
        getFileInput v_infile float $ v_params$->(var "tnt"),
        getFileInput v_infile float $ v_params$->(var "sdx"),
        getFileInput v_infile float $ v_params$->(var "sdy"),
        getFileInput v_infile float $ v_params$->(var "sdz"),
        getFileInput v_infile float $ v_params$->(var "pbtol"),
        closeFile v_infile
      ]
    ]      
  
l_filename, l_params, l_infile :: Label
l_filename = "filename"
l_params = "params"
l_infile = "infile"

p_filename, p_params :: Parameter
p_filename = param l_filename infile
p_params = param l_params (obj "InputParameters")

v_filename, v_params, v_infile :: Value
v_filename = var l_filename
v_params = var l_params
v_infile = var l_infile
