module Data.Drasil.Concepts.Math where

import Language.Drasil

gradient, norm_vect :: ConceptChunk 
ode :: CINP

--FIXME: Make some of these definitions better.
gradient  = dcc "gradient" (cn' "gradient") "gradient operator"
norm_vect = dcc "norm_vect" (cn' "normal") "unit outward normal vector for a surface"

--FIXME: use nounphrase instead of cn'
ode       = commonINP "ode" (cn' "Ordinary Differential Equation") "ODE"
