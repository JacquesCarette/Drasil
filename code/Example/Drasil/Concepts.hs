module Drasil.Concepts where

import Language.Drasil

program, os, matlab :: ConceptChunk
program = CC "program" (S "program")
os = CC "OS" (S "operating system")
matlab = CC "MATLAB" (S "MATLAB programming language")
