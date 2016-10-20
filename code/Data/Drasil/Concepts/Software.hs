module Data.Drasil.Concepts.Software where

import Language.Drasil

program, os, matlab, physLib, c :: ConceptChunk
c       = makeCC "C" "C programming language"
matlab  = CC "MATLAB" (S "MATLAB programming language")
os      = CC "OS" (S "operating system")
physLib = makeCC "physics library" ("A programming library which " ++
    "provides functions for modelling physical phenomenon.")
program = CC "program" (S "program")

-- MODULES Concepts (Maybe move to D.D.C.Software.Modules ?)

modHWHiding :: ConceptChunk
modHWHiding = CC "hardware hiding" 
  (S "Serves as a virtual hardware used by the rest of the system. This " :+:
   S "module provides the interface between the hardware and the software." :+:
   S " So, the system can use it to display outputs or to accept inputs.")
   
modBehavHiding :: ConceptChunk
modBehavHiding = CC "behaviour hiding"
    (S "Includes programs that provide externally visible behavior of " :+:
    S "the system as specified in the software requirements specification " :+:
    S "(SRS) documents. This module serves as a communication layer " :+:
    S "between the hardware-hiding module and the software decision " :+:
    S "module. The programs in this module will need to change if there " :+:
    S "are changes in the SRS.")
    
modControl :: ConceptChunk
modControl = CC "control" (S "Provides the main program.")

modSfwrDecision :: ConceptChunk
modSfwrDecision = CC "software decision"
    (S "Includes data structures and algorithms used in the system that " :+:
     S "do not provide direct interaction with the user.")