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
        ifCond [(a ?<= (litFloat 0.0), oneLiner (throw "InputError: a must be greater than 0"))] noElse,
        ifCond [(b ?<= (litFloat 0.0), oneLiner (throw "InputError: b must be greater than 0"))] noElse,
        ifCond [(asprat ?< (litFloat 1.0), oneLiner (throw "InputError: a/b cannot be less than 1.0"))] noElse,
        ifCond [(asprat ?> (litFloat 5.0), oneLiner (throw "InputError: a/b cannot be greater than 5.0"))] noElse,
        ifCond [(     t ?!= (litFloat 2.5) 
                  ?|| t ?!= (litFloat 2.7) 
                  ?|| t ?!= (litFloat 3.0) 
                  ?|| t ?!= (litFloat 4.0) 
                  ?|| t ?!= (litFloat 5.0) 
                  ?|| t ?!= (litFloat 6.0) 
                  ?|| t ?!= (litFloat 8.0) 
                  ?|| t ?!= (litFloat 10.0) 
                  ?|| t ?!= (litFloat 12.0) 
                  ?|| t ?!= (litFloat 16.0)
                  ?|| t ?!= (litFloat 19.0) 
                  ?|| t ?!= (litFloat 22.0),
          oneLiner (throw "InputError: t must be in [2.5,2.7,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,19.0,22.0]"))] noElse, 
        ifCond [(tnt ?<= (litFloat 0.0), oneLiner (throw "InputError: tnt must be greater than 0"))] noElse,
        ifCond [(wtnt ?< (litFloat 4.5), oneLiner (throw "InputError: wtnt cannot be less than 4.5"))] noElse,
        ifCond [(wtnt ?> (litFloat 910.0), oneLiner (throw "InputError: wtnt cannot be greater than 910.0"))] noElse,
        ifCond [(sd ?< (litFloat 6.0), oneLiner (throw "InputError: sd cannot be less than 6.0"))] noElse,
        ifCond [(sd ?> (litFloat 130.0), oneLiner (throw "InputError: sd cannot be greater than 130.0"))] noElse
      ]
    ]
  ]      

  
a, b, asprat, t, tnt, wtnt, sd :: Value
a = param_pre "a"
b = param_pre "b"
asprat = param_pre "asprat"
t = param_pre "t"
tnt = param_pre "tnt"
wtnt = param_pre "wtnt"
sd = param_pre "sd"

param_pre :: String -> Value
param_pre = (($->) (var "params")) . var