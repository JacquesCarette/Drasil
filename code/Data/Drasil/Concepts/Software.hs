module Data.Drasil.Concepts.Software where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (srs)
import Data.Drasil.Utils (foldlSent)
import Prelude hiding (id)
import Control.Lens ((^.))


c :: ConceptChunk
c = dcc "c" (pn "C") "C programming language"

physLib :: ConceptChunk
physLib = dcc "physLib"         (cnIES "physics library") 
  ("A programming library which" ++ 
  "provides functions for modelling physical phenomenon.")

program :: ConceptChunk
program = dcc "program"         (cn' "program")
  ("A series of coded software instructions to control the operation of a " ++
  "computer or other machine.")


-- MODULES Concepts (Maybe move to D.D.C.Software.Modules ?)

--FIXME: "hiding" is not a noun.
hwHiding :: ConceptChunk
hwHiding = dcc "hwHiding" (cn "hardware hiding") (
  "Hides the exact details of the hardware, and provides a uniform interface" ++
  " for the rest of the system to use.")

modBehavHiding :: ConceptChunk
modBehavHiding = dccWDS "modBehavHiding" (cn "behaviour hiding") (foldlSent
  [S "Includes programs that provide externally visible behaviour of the", 
  S "system as specified in the", (phrase (srs ^. term)),
  S "(" :+: (short srs) :+: S ") documents. This module",
  S "serves as a communication layer between the hardware-hiding module",
  S "and the software decision module. The programs in this module will need",
  S "to change if there are changes in the", (short srs)])

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
  
modInputParam :: ConceptChunk
modInputParam = dccWDS "modInputParam" (cn' "input parameter") (foldlSent
  [S "Stores the parameters needed",
  S "for the program, including material properties" `sC`
  S "processing conditions, and numerical parameters. The",
  S "values can be read as needed. This module knows how",
  S "many parameters it stores"])

modInputConstraint :: ConceptChunk
modInputConstraint = dccWDS "modInputConstraint" (cn' "input constraint") 
  (foldlSent [S "Defines the constraints on the input data and gives an error if",
  S "a constraint is violated"])

modInputVerif :: ConceptChunk
modInputVerif = dccWDS "modInputVerif" (cn' "input verification") (foldlSent
  [S "Verifies that the input",
  S "parameters comply with physical and software", 
  S "constraints. Throws an error if a parameter violates a",
  S "physical constraint. Throws a warning if a parameter",
  S "violates a software constraint"])

modDerivedVal :: ConceptChunk
modDerivedVal = dccWDS "modDerivedVal" (cn' "derived value") (foldlSent
  [S "Defines the equations transforming the initial inputs into derived",
  S "quantities"])

modInterpolation :: ConceptChunk
modInterpolation = dccWDS "modInterpolation" (cn "interpolation") (foldlSent
  [S "Provides the equations that take the input parameters and",
  S "interpolation data and return an interpolated value"])

modInterpDatum :: ConceptChunk
modInterpDatum = dccWDS "modInterpDatum" (cn "interpolation datum") (foldlSent
  [S "Converts the input interpolation data into the data structure used",
  S "by the interpolation module"])

{-- Concept Chunks for Modules  --}

mod_seq_serv :: ConceptChunk
mod_seq_serv = dccWDS "mod_seq_serv" (cn' "sequence data structure")
    (S "Provides array manipulation operations, such as building an array" `sC`
    S "accessing a specific entry, slicing an array, etc.")

mod_linked_serv :: ConceptChunk
mod_linked_serv = dccWDS "mod_linked_serv" (cn' "linked data structure")
    (S "Provides tree manipulation operations, such as building a tree" `sC`
    S "accessing a specific entry, etc.")

mod_assoc_serv :: ConceptChunk
mod_assoc_serv = dccWDS "mod_assoc_serv" (cn' "associative data structure")
    (S "Provides operations on hash tables, such as building a hash table" `sC`
    S "accessing a specific entry, etc.")

mod_vector_serv :: ConceptChunk
mod_vector_serv = dccWDS "mod_vector_serv" (cn' "vector")
    (S "Provides vector operations such as addition, scalar and vector" +:+
    S "multiplication, dot and cross products, rotations, etc.")
    
mod_plot_desc :: ConceptChunk
mod_plot_desc = dcc "mod_plot_desc" (cn' "plotting") "Provides a plot function."

mod_outputf_desc_fun :: Sentence -> ConceptChunk
mod_outputf_desc_fun desc = dccWDS "mod_outputf_desc" (cn' "output format")
    (S "Outputs the results of the calculations, including the" +:+ desc)
