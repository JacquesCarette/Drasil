module Data.Drasil.Concepts.Math where

import Language.Drasil

gradient, norm_vect :: ConceptChunk

gradient = makeCC "gradient" "gradient operator"
norm_vect = makeCC "n_vect" "unit outward normal vector for a surface"