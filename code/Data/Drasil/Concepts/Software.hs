module Data.Drasil.Concepts.Software where

import Language.Drasil

program, os, matlab, physLib, c :: ConceptChunk
c       = makeCC "C" "C programming language"
matlab  = CC "MATLAB" (S "MATLAB programming language")
os      = CC "OS" (S "operating system")
physLib = makeCC "physics library" ("A programming library which " ++
    "provides functions for modelling physical phenomenon.")
program = CC "program" (S "program")
