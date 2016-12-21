module Data.Drasil.Concepts.Software where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (srs)
import Prelude hiding (id)
import Control.Lens ((^.))
import Data.Char (toLower)

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
modBehavHiding = CC "behaviour hiding" (S "Includes programs that provide " :+:
                 S "externally visible behaviour of the system as specified" :+:
                 S " in the " :+: (sMap (map toLower) (srs ^. term)) :+:
                 S " (" :+: S (srs ^. id) :+: S ") documents. This module" :+:
                 S " serves as a communication layer between the hardware-" :+:
                 S "hiding module and the software decision module. The " :+:
                 S "programs in this module will need to change if there " :+:
                 S "are changes in the " :+: S (srs ^. id) :+: S ".")
    
modControl :: ConceptChunk
modControl = CC "control" (S "Provides the main program.")

modSfwrDecision :: ConceptChunk
modSfwrDecision = CC "software decision"
    (S "Includes data structures and algorithms used in the system that " :+:
     S "do not provide direct interaction with the user.")
     
modInputFormat :: ConceptChunk
modInputFormat = CC "input format"
  (S "Converts the input data into the data structure used by the input " :+:
   S "parameters module.")
   
modInputParams :: ConceptChunk
modInputParams = CC "input parameters" (S "Stores the parameters needed " :+:
                  S "for the program, including material properties, " :+:
                  S "processing conditions, and numerical parameters. The " :+:
                  S "values can be read as needed. This module knows how " :+:
                  S "many parameters it stores.")
   
modInputConstraints :: ConceptChunk
modInputConstraints = CC "input constraints"
  (S "Defines the constraints on the input data and gives an error if " :+:
   S "a constraint is violated.")
   
modInputVerif :: ConceptChunk
modInputVerif = CC "input verification" (S "Verifies that the input " :+:
                  S "parameters comply with physical and software " :+: 
                  S "constraints. Throws an error if a parameter violates a" :+:
                  S " physical constraint. Throws a warning if a parameter " :+:
                  S "violates a software constraint.")
   
modDerivedVals :: ConceptChunk
modDerivedVals = CC "derived values"
  (S "Defines the equations transforming the initial inputs into derived " :+:
   S "quantities.")
   
modInterpolation :: ConceptChunk
modInterpolation = CC "interpolation"
  (S "Provides the equations that take the input parameters and " :+:
   S "interpolation data and return an interpolated value.")
   
modInterpData :: ConceptChunk
modInterpData = CC "interpolation data"
  (S "Converts the input interpolation data into the data structure used " :+:
   S "by the interpolation module.")