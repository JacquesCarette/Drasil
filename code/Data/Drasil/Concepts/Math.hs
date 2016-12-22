module Data.Drasil.Concepts.Math where

import Language.Drasil

gradient, norm_vect :: ConceptChunk
ode :: NamedChunk

--FIXME: Make some of these definitions better.
gradient  = makeDCC "gradient" "gradient" "gradient operator"
norm_vect = makeDCC "norm_vect" "normal" "unit outward normal vector for a surface"
ode       = makeCC "ODE" "Ordinary Differential Equation"
