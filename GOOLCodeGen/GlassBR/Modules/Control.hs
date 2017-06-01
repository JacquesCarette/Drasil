module Modules.Control (control) where

import Language.Drasil.Code
import Defs
import Prelude hiding (return)

control :: Module
control = buildModule "Control" [] [] [main_func] []

main_func :: FunctionDecl
main_func = mainMethod 
  [
    block [
      
    ] 
  ]