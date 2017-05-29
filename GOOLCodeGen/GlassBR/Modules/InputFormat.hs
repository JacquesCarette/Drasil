module Modules.InputFormat (inputFormat) where

import Language.Drasil.Code

-- TODO:  add ability to specify libraries in gool
-- TODO:  add gool support for non-object functions;  don't need a class for this
inputFormat :: Module
inputFormat = buildModule "InputFormat" ["numpy"] [] [inputFormatFunc] []

inputFormatFunc :: FunctionDecl
inputFormatFunc = pubMethod methodTypeVoid "get_input" (params [("filename", string), ("params", obj "InputParameters")]) 
    [ 
      block [
        -- TODO:  improve gool file IO (some languages use object methods, some not...
        --        probably best to add as new IO Statement)
        varDecDef "infile" infile (funcApp' "open" [var "filename", litString "r"]),
        (var "params")$->(var "a") &= readfl64,
        (var "params")$->(var "b") &= readfl64,
        (var "params")$->(var "t") &= readfl64,
        (var "params")$->(var "gt") &= readline,
        (var "params")$->(var "w") &= readfl64,
        (var "params")$->(var "tnt") &= readfl64,
        (var "params")$->(var "sdx") &= readfl64,
        (var "params")$->(var "sdy") &= readfl64,
        (var "params")$->(var "sdz") &= readfl64,
        (var "params")$->(var "pbtol") &= readfl64,
        valStmt (objMethodCall (var "infile") "close" [])
      ]
    ]      
  
  
-- float64 from numpy library
--   this should come from a library database
readfl64 :: Value
readfl64 = funcApp "numpy" "float64" [readline]  
  
readline :: Value
readline = objMethodCall (var "infile") "readline" []
