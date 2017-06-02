module Modules.InputConstraints (inputConstraints) where

import Language.Drasil.Code
import Defs

-- TODO:  add gool support for non-object functions;  don't need a class for this
inputConstraints :: Module
inputConstraints = buildModule "InputConstraints" [] [] [check_constraints_func] []

check_constraints_func :: FunctionDecl
check_constraints_func = pubMethod methodTypeVoid "check_constraints" [p_params] 
    [ 
      block [
        ifCond [(v_params$->v_a ?<= (litFloat 0.0), oneLiner (throw "InputError: a must be greater than 0"))] noElse,
        ifCond [(v_params$->v_b ?<= (litFloat 0.0), oneLiner (throw "InputError: b must be greater than 0"))] noElse,
        ifCond [(v_params$->v_asprat ?< (litFloat 1.0), oneLiner (throw "InputError: a/b cannot be less than 1.0"))] noElse,
        ifCond [(v_params$->v_asprat ?> (litFloat 5.0), oneLiner (throw "InputError: a/b cannot be greater than 5.0"))] noElse,
        ifCond [( (?!) (v_params$->v_t ?== (litFloat 2.5) 
                  ?|| v_params$->v_t ?== (litFloat 2.7) 
                  ?|| v_params$->v_t ?== (litFloat 3.0) 
                  ?|| v_params$->v_t ?== (litFloat 4.0) 
                  ?|| v_params$->v_t ?== (litFloat 5.0) 
                  ?|| v_params$->v_t ?== (litFloat 6.0) 
                  ?|| v_params$->v_t ?== (litFloat 8.0) 
                  ?|| v_params$->v_t ?== (litFloat 10.0) 
                  ?|| v_params$->v_t ?== (litFloat 12.0) 
                  ?|| v_params$->v_t ?== (litFloat 16.0)
                  ?|| v_params$->v_t ?== (litFloat 19.0) 
                  ?|| v_params$->v_t ?== (litFloat 22.0)),
          oneLiner (throw "InputError: t must be in [2.5,2.7,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,19.0,22.0]"))] noElse, 
        ifCond [(v_params$->v_tnt ?<= (litFloat 0.0), oneLiner (throw "InputError: tnt must be greater than 0"))] noElse,
        ifCond [(v_params$->v_wtnt ?< (litFloat 4.5), oneLiner (throw "InputError: wtnt cannot be less than 4.5"))] noElse,
        ifCond [(v_params$->v_wtnt ?> (litFloat 910.0), oneLiner (throw "InputError: wtnt cannot be greater than 910.0"))] noElse,
        ifCond [(v_params$->v_sd ?< (litFloat 6.0), oneLiner (throw "InputError: sd cannot be less than 6.0"))] noElse,
        ifCond [(v_params$->v_sd ?> (litFloat 130.0), oneLiner (throw "InputError: sd cannot be greater than 130.0"))] noElse
      ]
    ]