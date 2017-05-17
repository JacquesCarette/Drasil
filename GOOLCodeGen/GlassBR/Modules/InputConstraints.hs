module Modules.InputConstraints (inputConstraints) where

import Language.Drasil.Code

inputConstraints :: Class
inputConstraints = pubClass
  "InputConstraints"
  Nothing
  []
  [ pubMethod methodTypeVoid "check_constraints" (params [("params", obj "InputParameters")]) 
    [ 
      block [
        ifelse [ (var "params" $-> var "a" ?<= (litFloat 0.0), oneLiner (excThrow "InputError: a must be greater than 0")) ] 
        [ block [] ]
      ]
    ]
  ]      

