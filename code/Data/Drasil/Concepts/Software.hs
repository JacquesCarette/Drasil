module Data.Drasil.Concepts.Software where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (srs)
import Prelude hiding (id)
import Control.Lens ((^.))

program, c, matlab, physLib :: ConceptChunk
os :: NamedChunk

c       = dcc "c" "C" "C programming language"
matlab  = dcc "matlab" "MATLAB" "MATLAB programming language"
os      = nc' "os" "operating system" "OS"
physLib = dcc "physLib" "physics library" ("A programming library which " ++
  "provides functions for modelling physical phenomenon.")
program = dcc "program" "program" 
  ("a series of coded software instructions to control the operation of a " ++
  "computer or other machine.")

-- MODULES Concepts (Maybe move to D.D.C.Software.Modules ?)

hwHiding :: ConceptChunk
hwHiding = dcc "hwHiding" "hardware hiding" (
  "Hides the exact details of the hardware, and provides a uniform interface" ++
  "for the rest of the system to use.")

modBehavHiding :: ConceptChunk
modBehavHiding = dccWDS "modBehavHiding" "behaviour hiding" (
  S "Includes programs that provide externally visible behaviour of the" +:+ 
  S "system as specified in the" +:+ (sLower (srs ^. term)) +:+
  S "(" :+: (short srs) :+: S ") documents. This module" +:+
  S "serves as a communication layer between the hardware-hiding module" +:+
  S "and the software decision module. The programs in this module will need" +:+
  S "to change if there are changes in the" +:+. (short srs))

modControl :: ConceptChunk
modControl = dcc "modControl" "control" "Provides the main program."

modSfwrDecision :: ConceptChunk
modSfwrDecision = dcc "modSfwrDecision" "software decision" 
  ("Includes data structures and algorithms used in the system that do not " ++
  "provide direct interaction with the user.")

modInputFormat :: ConceptChunk
modInputFormat = dcc "modInputFormat" "input format" ("Converts the input " ++
  "data into the data structure used by the input parameters module.")
  
modInputParams :: ConceptChunk
modInputParams = dccWDS "modInputParams" "input parameters" (
  S "Stores the parameters needed " :+:
  S "for the program, including material properties, " :+:
  S "processing conditions, and numerical parameters. The " :+:
  S "values can be read as needed. This module knows how " :+:
  S "many parameters it stores.")

modInputConstraints :: ConceptChunk
modInputConstraints = dccWDS "modInputConstraints" "input constraints" (
  S "Defines the constraints on the input data and gives an error if " :+:
  S "a constraint is violated.")

modInputVerif :: ConceptChunk
modInputVerif = dccWDS "modInputVerif" "input verification" (
  S "Verifies that the input " :+:
  S "parameters comply with physical and software " :+: 
  S "constraints. Throws an error if a parameter violates a" :+:
  S " physical constraint. Throws a warning if a parameter " :+:
  S "violates a software constraint.")

modDerivedVals :: ConceptChunk
modDerivedVals = dccWDS "modDerivedVals" "derived values" (
  S "Defines the equations transforming the initial inputs into derived " :+:
  S "quantities.")

modInterpolation :: ConceptChunk
modInterpolation = dccWDS "modInterpolation" "interpolation" (
  S "Provides the equations that take the input parameters and " :+:
  S "interpolation data and return an interpolated value.")

modInterpData :: ConceptChunk
modInterpData = dccWDS "modInterpData" "interpolation data" (
  S "Converts the input interpolation data into the data structure used " :+:
  S "by the interpolation module.")
