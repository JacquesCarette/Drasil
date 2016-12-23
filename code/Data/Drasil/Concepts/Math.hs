module Data.Drasil.Concepts.Math where

import Language.Drasil

gradient, norm_vect, ode :: ConceptChunk

--FIXME: Make some of these definitions better.
gradient  = dcc "gradient" "gradient" "gradient operator"
norm_vect = dcc "norm_vect" "normal" "unit outward normal vector for a surface"
ode       = dcc "ode" "ODE" "Ordinary Differential Equation"
