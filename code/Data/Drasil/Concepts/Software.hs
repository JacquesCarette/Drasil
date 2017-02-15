module Data.Drasil.Concepts.Software where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (srs)
import Prelude hiding (id)
import Control.Lens ((^.))
import Data.Char (toLower)

program, c, matlab, physLib, os :: ConceptChunk

c       = dcc "c" "C" "C programming language"
matlab  = dcc "matlab" "MATLAB" "MATLAB programming language"
os      = dcc "os" "OS" "operating system"
physLib = dcc "physLib" "physics library" ("A programming library which " ++
  "provides functions for modelling physical phenomenon.")
program = dcc "program" "program" 
  ("a series of coded software instructions to control the operation of a " ++
  "computer or other machine.")

-- MODULES Concepts (Maybe move to D.D.C.Software.Modules ?)

hwHiding :: ConceptChunk
hwHiding = dcc "hardware hiding" "hardware hiding"
  ("Hides the exact details of the hardware, and provides a uniform interface" ++
   "for the rest of the system to use.")
   
modBehavHiding :: NamedChunk
modBehavHiding = ncWDS "behaviour hiding" (S "Includes programs that provide " :+:
                 S "externally visible behaviour of the system as specified" :+:
                 S " in the " :+: (sMap (map toLower) (srs ^. defn)) :+:
                 S " (" :+: (srs ^. term) :+: S ") documents. This module" :+:
                 S " serves as a communication layer between the hardware-" :+:
                 S "hiding module and the software decision module. The " :+:
                 S "programs in this module will need to change if there " :+:
                 S "are changes in the " :+: (srs ^. term) :+: S ".")
    
modControl :: NamedChunk
modControl = ncWDS "control" (S "Provides the main program.")

modSfwrDecision :: NamedChunk
modSfwrDecision = ncWDS "software decision"
    (S "Includes data structures and algorithms used in the system that " :+:
     S "do not provide direct interaction with the user.")
     
modInputFormat :: NamedChunk
modInputFormat = ncWDS "input format"
  (S "Converts the input data into the data structure used by the input " :+:
   S "parameters module.")
   
modInputParams :: NamedChunk
modInputParams = ncWDS "input parameters" (S "Stores the parameters needed " :+:
                  S "for the program, including material properties, " :+:
                  S "processing conditions, and numerical parameters. The " :+:
                  S "values can be read as needed. This module knows how " :+:
                  S "many parameters it stores.")
   
modInputConstraints :: NamedChunk
modInputConstraints = ncWDS "input constraints"
  (S "Defines the constraints on the input data and gives an error if " :+:
   S "a constraint is violated.")
   
modInputVerif :: NamedChunk
modInputVerif = ncWDS "input verification" (S "Verifies that the input " :+:
                  S "parameters comply with physical and software " :+: 
                  S "constraints. Throws an error if a parameter violates a" :+:
                  S " physical constraint. Throws a warning if a parameter " :+:
                  S "violates a software constraint.")
   
modDerivedVals :: NamedChunk
modDerivedVals = ncWDS "derived values"
  (S "Defines the equations transforming the initial inputs into derived " :+:
   S "quantities.")
   
modInterpolation :: NamedChunk
modInterpolation = ncWDS "interpolation"
  (S "Provides the equations that take the input parameters and " :+:
   S "interpolation data and return an interpolated value.")
   
modInterpData :: NamedChunk
modInterpData = ncWDS "interpolation data"
  (S "Converts the input interpolation data into the data structure used " :+:
   S "by the interpolation module.")
