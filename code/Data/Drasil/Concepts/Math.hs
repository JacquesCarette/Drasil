module Data.Drasil.Concepts.Math where

import Language.Drasil

gradient, norm_vect :: ConceptChunk 
ode :: CINP

--FIXME: Make some of these definitions better.
gradient  = dcc "gradient" "gradient" "gradient operator"
norm_vect = dcc "norm_vect" "normal" "unit outward normal vector for a surface"

ode       = commonINP "ode" (cn' "Ordinary Differential Equation") "ODE"
