module Drasil.Concepts where

import Language.Drasil

program, os :: ConceptChunk
program = CC "HGHC" (S "HGHC program")
os = CC "OS" (S "operating system")
