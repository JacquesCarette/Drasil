module Modules.DerivedValues (derivedValues) where

import Language.Drasil.Code
import Defs

derivedValues :: Module
derivedValues = buildModule "DerivedValues" [] [] [derived_params_func] []

derived_params_func :: FunctionDecl
derived_params_func = pubMethod methodTypeVoid "derived_params" [p_params]
    [ 
      block [
        v_params$->v_asprat &= v_params$->v_a #/ v_params$->v_b,
        v_params$->v_sd &= (#/^) (v_params$->v_sdx #^ (litFloat 2.0) #+ v_params$->v_sdy #^ (litFloat 2.0) #+ v_params$->v_sdz #^ (litFloat 2.0)),
        v_params$->v_ldf &= (v_params$->v_td #/ litFloat 60.0) #^ (v_params$->v_m #/ litFloat 16.0),
        v_params$->v_wtnt &= v_params$->v_w #* v_params$->v_tnt,
        ifCond [
          (v_params$->v_t ?== litFloat 2.50, oneLiner (v_params$->v_h &= litFloat 2.16)),
          (v_params$->v_t ?== litFloat 2.70, oneLiner (v_params$->v_h &= litFloat 2.59)),
          (v_params$->v_t ?== litFloat 3.00, oneLiner (v_params$->v_h &= litFloat 2.92)),
          (v_params$->v_t ?== litFloat 4.00, oneLiner (v_params$->v_h &= litFloat 3.78)),
          (v_params$->v_t ?== litFloat 5.00, oneLiner (v_params$->v_h &= litFloat 4.57)),
          (v_params$->v_t ?== litFloat 6.00, oneLiner (v_params$->v_h &= litFloat 5.56)),
          (v_params$->v_t ?== litFloat 8.00, oneLiner (v_params$->v_h &= litFloat 7.42)),
          (v_params$->v_t ?== litFloat 10.00, oneLiner (v_params$->v_h &= litFloat 9.02)),
          (v_params$->v_t ?== litFloat 12.00, oneLiner (v_params$->v_h &= litFloat 11.91)),
          (v_params$->v_t ?== litFloat 16.00, oneLiner (v_params$->v_h &= litFloat 15.09)),
          (v_params$->v_t ?== litFloat 19.00, oneLiner (v_params$->v_h &= litFloat 18.26)),
          (v_params$->v_t ?== litFloat 22.00, oneLiner (v_params$->v_h &= litFloat 21.44))
        ] noElse,
        ifCond [
          (v_params$->v_gt ?== litInt 1, oneLiner (v_params$->v_gtf &= litFloat 1.0)),
          (v_params$->v_gt ?== litInt 2, oneLiner (v_params$->v_gtf &= litFloat 2.0)),
          (v_params$->v_gt ?== litInt 3, oneLiner (v_params$->v_gtf &= litFloat 4.0))
        ] noElse
      ]
    ]     