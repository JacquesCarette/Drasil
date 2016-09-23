module Drasil.Concepts where

import Language.Drasil

program, os, matlab :: ConceptChunk
program = CC "HGHC" (S "HGHC program")
os = CC "OS" (S "operating system")
matlab = CC "MATLAB" (S "MATLAB programming language")
