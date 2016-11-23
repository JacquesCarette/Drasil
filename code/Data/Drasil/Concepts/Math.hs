module Data.Drasil.Concepts.Math where

import Language.Drasil

gradient :: DefinedTerm
norm_vect, ode :: ConceptChunk

--FIXME: Make some of these definitions better.
gradient  = makeDCC "gradient" "gradient" "gradient operator"
norm_vect = makeCC "n_vect" "unit outward normal vector for a surface"
ode       = makeCC "ODE" "Ordinary Differential Equation"
