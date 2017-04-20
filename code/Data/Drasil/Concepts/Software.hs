module Data.Drasil.Concepts.Software where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (srs)
import Prelude hiding (id)
import Control.Lens ((^.))

program, c, physLib :: ConceptChunk
os, matlab :: NPNC

c       = dcc "c" (pn "C") "C programming language"
matlab  = npnc' "matlab" (pn' "MATLAB programming language") "MATLAB"
os      = npnc' "os" (cn' "operating system") "OS"
physLib = dcc "physLib" (cnIES "physics library") 
  ("A programming library which " ++
  "provides functions for modelling physical phenomenon.")
program = dcc "program" (cn' "program")
  ("a series of coded software instructions to control the operation of a " ++
  "computer or other machine.")

-- MODULES Concepts (Maybe move to D.D.C.Software.Modules ?)

--FIXME: "hiding" is not a noun.
hwHiding :: ConceptChunk
hwHiding = dcc "hwHiding" (cn "hardware hiding") (
  "Hides the exact details of the hardware, and provides a uniform interface" ++
  " for the rest of the system to use.")

--FIXME: remove "sLower"
modBehavHiding :: ConceptChunk
modBehavHiding = dccWDS "modBehavHiding" (cn "behaviour hiding") (
  S "Includes programs that provide externally visible behaviour of the" +:+ 
  S "system as specified in the" +:+ (sLower (phrase (srs ^. term))) +:+
  S "(" :+: (short srs) :+: S ") documents. This module" +:+
  S "serves as a communication layer between the hardware-hiding module" +:+
  S "and the software decision module. The programs in this module will need" +:+
  S "to change if there are changes in the" +:+. (short srs))

modControl :: ConceptChunk
modControl = dcc "modControl" (cn' "control") "Provides the main program."

modSfwrDecision :: ConceptChunk
modSfwrDecision = dcc "modSfwrDecision" (cn' "software decision")
  ("Includes data structures and algorithms used in the system that do not " ++
  "provide direct interaction with the user.")

modInputFormat :: ConceptChunk
modInputFormat = dcc "modInputFormat" (cn' "input format")
  ("Converts the input data into the data structure " ++
  "used by the input parameters module.")
  
--FIXME: switch to "input parameter" and use plural.
modInputParams :: ConceptChunk
modInputParams = dccWDS "modInputParams" (cn' "input parameters") (
  S "Stores the parameters needed " :+:
  S "for the program, including material properties, " :+:
  S "processing conditions, and numerical parameters. The " :+:
  S "values can be read as needed. This module knows how " :+:
  S "many parameters it stores.")

--FIXME: switch to "input constraint" and use plural.
modInputConstraints :: ConceptChunk
modInputConstraints = dccWDS "modInputConstraints" (cn' "input constraints") (
  S "Defines the constraints on the input data and gives an error if " :+:
  S "a constraint is violated.")

modInputVerif :: ConceptChunk
modInputVerif = dccWDS "modInputVerif" (cn' "input verification") (
  S "Verifies that the input " :+:
  S "parameters comply with physical and software " :+: 
  S "constraints. Throws an error if a parameter violates a" :+:
  S " physical constraint. Throws a warning if a parameter " :+:
  S "violates a software constraint.")

--FIXME: switch to "derived value" and use plural.
modDerivedVals :: ConceptChunk
modDerivedVals = dccWDS "modDerivedVals" (cn' "derived values") (
  S "Defines the equations transforming the initial inputs into derived " :+:
  S "quantities.")

modInterpolation :: ConceptChunk
modInterpolation = dccWDS "modInterpolation" (cn "interpolation") (
  S "Provides the equations that take the input parameters and " :+:
  S "interpolation data and return an interpolated value.")

--FIXME: switch to a combination of interpolation and datum, then use plural.
modInterpData :: ConceptChunk
modInterpData = dccWDS "modInterpData" (cn "interpolation data") (
  S "Converts the input interpolation data into the data structure used " :+:
  S "by the interpolation module.")
