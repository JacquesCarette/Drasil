module Modules.OutputFormat (outputFormat) where

import Language.Drasil.Code

outputFormat :: Module
outputFormat = buildModule "OutputFormat" [] [] [display_output_func] []

display_output_func :: FunctionDecl
display_output_func = pubMethod methodTypeVoid "display_output" []
    [ 
      block [
     
      ]
    ]      
