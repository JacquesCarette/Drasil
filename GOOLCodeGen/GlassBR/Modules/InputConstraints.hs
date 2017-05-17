module Modules.InputConstraints (inputConstraints) where

import Language.Drasil.Code

-- TODO:  add gool support for non-object functions;  don't need a class for this
inputConstraints :: Class
inputConstraints = pubClass
  "InputConstraints"
  Nothing
  []
  [ pubMethod methodTypeVoid "check_constraints" (params [("params", obj "InputParameters")]) 
    [ 
      block [
        -- TODO:  add gool support for "else-less" if statements
        ifCond [ 
          (var "params" $-> var "a" ?<= (litFloat 0.0), oneLiner (throw "InputError: a must be greater than 0")),
          (var "params" $-> var "b" ?<= (litFloat 0.0), oneLiner (throw "InputError: b must be greater than 0")),
          (var "params" $-> var "asprat" ?< (litFloat 1.0), oneLiner (throw "InputError: a/b cannot be less than 1.0")),
          (var "params" $-> var "asprat" ?> (litFloat 5.0), oneLiner (throw "InputError: a/b cannot be greater than 5.0")),
          -- TODO: missing a condition, gool needs logical operators
          (var "params" $-> var "tnt" ?<= (litFloat 0.0), oneLiner (throw "InputError: tnt must be greater than 0")),
          (var "params" $-> var "wtnt" ?< (litFloat 4.5), oneLiner (throw "InputError: wtnt cannot be less than 4.5")),
          (var "params" $-> var "wtnt" ?> (litFloat 910.0), oneLiner (throw "InputError: wtnt cannot be greater than 910.0")),
          (var "params" $-> var "sd" ?< (litFloat 6.0), oneLiner (throw "InputError: sd cannot be less than 6.0")),
          (var "params" $-> var "sd" ?> (litFloat 130.0), oneLiner (throw "InputError: sd cannot be greater than 130.0"))
        ] 
        [ block [] ]
      ]
    ]
  ]      

