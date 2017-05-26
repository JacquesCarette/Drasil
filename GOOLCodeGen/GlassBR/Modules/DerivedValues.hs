module Modules.DerivedValues (derivedValues) where

import Language.Drasil.Code

-- TODO:  add gool support for non-object functions;  don't need a class for this
derivedValues :: Module
derivedValues = buildModule "DerivedValues" [] [] [derivedValuesFunc] []

derivedValuesFunc :: FunctionDecl
derivedValuesFunc = pubMethod methodTypeVoid "derived_params" (params [("params", obj "InputParameters")]) 
    [ 
      block [
        asprat &= a #/ b,
        sd &= (#/^) (sdx #^ (litFloat 2.0) #+ sdy #^ (litFloat 2.0) #+ sdz #^ (litFloat 2.0)),
        ldf &= (td #/ litFloat 60.0) #^ (m #/ litFloat 16.0),
        wtnt &= w #* tnt,
        ifCond [
          (t ?== litFloat 2.50, oneLiner (h &= litFloat 2.16)),
          (t ?== litFloat 2.70, oneLiner (h &= litFloat 2.59)),
          (t ?== litFloat 3.00, oneLiner (h &= litFloat 2.92)),
          (t ?== litFloat 4.00, oneLiner (h &= litFloat 3.78)),
          (t ?== litFloat 5.00, oneLiner (h &= litFloat 4.57)),
          (t ?== litFloat 6.00, oneLiner (h &= litFloat 5.56)),
          (t ?== litFloat 8.00, oneLiner (h &= litFloat 7.42)),
          (t ?== litFloat 10.00, oneLiner (h &= litFloat 9.02)),
          (t ?== litFloat 12.00, oneLiner (h &= litFloat 11.91)),
          (t ?== litFloat 16.00, oneLiner (h &= litFloat 15.09)),
          (t ?== litFloat 19.00, oneLiner (h &= litFloat 18.26)),
          (t ?== litFloat 22.00, oneLiner (h &= litFloat 21.44))
        ] noElse,
        ifCond [
          (gt ?== litString "AN" ?|| gt ?== litString "an", oneLiner (gtf &= litFloat 1.0)),
          (gt ?== litString "HS" ?|| gt ?== litString "hs", oneLiner (gtf &= litFloat 2.0)),
          (gt ?== litString "FT" ?|| gt ?== litString "ft", oneLiner (gtf &= litFloat 4.0))
        ] noElse
      ]
    ]     
  
        
a, b, asprat, sd, sdx, sdy, sdz, ldf, td, m, wtnt, w, tnt, t, h, gt, gtf :: Value
a = param_pre "a"
b = param_pre "b"
asprat = param_pre "asprat"
sd = param_pre "sd"
sdx = param_pre "sdx"
sdy = param_pre "sdy"
sdz = param_pre "sdz"
ldf = param_pre "ldf"
td = param_pre "td"
m = param_pre "m"
wtnt = param_pre "wtnt"
w = param_pre "w"
tnt = param_pre "tnt"
t = param_pre "t"
h = param_pre "h"
gt = param_pre "gt"
gtf = param_pre "gtf"

param_pre :: String -> Value
param_pre = (($->) (var "params")) . var