module Data.Drasil.Concepts.Software where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (srs)
import Prelude hiding (id)
import Control.Lens ((^.))
import Data.Char (toLower)

program, c :: NamedChunk
matlab, physLib, os :: ConceptChunk
c       = makeCC "C" "C programming language"
matlab  = dcc "matlab" "MATLAB" "MATLAB programming language"
os      = dcc "os" "OS" "operating system"
physLib = dcc "physLib" "physics library" ("A programming library which " ++
    "provides functions for modelling physical phenomenon.")
program = CC "program" (S "program")

-- MODULES Concepts (Maybe move to D.D.C.Software.Modules ?)

modHWHiding :: NamedChunk
modHWHiding = CC "hardware hiding" 
  (S "Serves as a virtual hardware used by the rest of the system. This " :+:
   S "module provides the interface between the hardware and the software." :+:
   S " So, the system can use it to display outputs or to accept inputs.")
   
modBehavHiding :: NamedChunk
modBehavHiding = CC "behaviour hiding" (S "Includes programs that provide " :+:
                 S "externally visible behaviour of the system as specified" :+:
                 S " in the " :+: (sMap (map toLower) (srs ^. defn)) :+:
                 S " (" :+: (srs ^. term) :+: S ") documents. This module" :+:
                 S " serves as a communication layer between the hardware-" :+:
                 S "hiding module and the software decision module. The " :+:
                 S "programs in this module will need to change if there " :+:
                 S "are changes in the " :+: (srs ^. term) :+: S ".")
    
modControl :: NamedChunk
modControl = CC "control" (S "Provides the main program.")

modSfwrDecision :: NamedChunk
modSfwrDecision = CC "software decision"
    (S "Includes data structures and algorithms used in the system that " :+:
     S "do not provide direct interaction with the user.")
     
modInputFormat :: NamedChunk
modInputFormat = CC "input format"
  (S "Converts the input data into the data structure used by the input " :+:
   S "parameters module.")
   
modInputParams :: NamedChunk
modInputParams = CC "input parameters" (S "Stores the parameters needed " :+:
                  S "for the program, including material properties, " :+:
                  S "processing conditions, and numerical parameters. The " :+:
                  S "values can be read as needed. This module knows how " :+:
                  S "many parameters it stores.")
   
modInputConstraints :: NamedChunk
modInputConstraints = CC "input constraints"
  (S "Defines the constraints on the input data and gives an error if " :+:
   S "a constraint is violated.")
   
modInputVerif :: NamedChunk
modInputVerif = CC "input verification" (S "Verifies that the input " :+:
                  S "parameters comply with physical and software " :+: 
                  S "constraints. Throws an error if a parameter violates a" :+:
                  S " physical constraint. Throws a warning if a parameter " :+:
                  S "violates a software constraint.")
   
modDerivedVals :: NamedChunk
modDerivedVals = CC "derived values"
  (S "Defines the equations transforming the initial inputs into derived " :+:
   S "quantities.")
   
modInterpolation :: NamedChunk
modInterpolation = CC "interpolation"
  (S "Provides the equations that take the input parameters and " :+:
   S "interpolation data and return an interpolated value.")
   
modInterpData :: NamedChunk
modInterpData = CC "interpolation data"
  (S "Converts the input interpolation data into the data structure used " :+:
   S "by the interpolation module.")