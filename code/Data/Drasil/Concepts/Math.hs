module Data.Drasil.Concepts.Math where

import Language.Drasil

gradient, norm_vect :: ConceptChunk 
ode :: CI

--FIXME: Make some of these definitions better.
gradient  = dcc "gradient" "gradient" "gradient operator"
norm_vect = dcc "norm_vect" "normal" "unit outward normal vector for a surface"

ode       = commonidea "ode" "Ordinary Differential Equation" "ODE"
