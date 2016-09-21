module Drasil.SSP.SSPConcepts where

import Language.Drasil

program, os, matlab :: ConceptChunk
program = CC "SSP" (S "SSP program")
os = CC "OS" (S "operating system")
matlab = CC "MATLAB" (S "MATLAB programming language")
