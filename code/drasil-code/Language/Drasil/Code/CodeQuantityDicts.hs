-- | Defines QuantityDicts needed in the code generation stage.
module Language.Drasil.Code.CodeQuantityDicts where

import Language.Drasil
import Language.Drasil.Display

-- Variable for the input file's name.
inFileName :: QuantityDict
inFileName = implVar "inFileName" 
  (nounPhrase "name of the input file" "names of the input files") String
  (Label "filename")

-- Variable for an object of the InputParameters class.
inParams :: QuantityDict
inParams = implVar "inParams" (nounPhrase 
  "structure holding the input values" "structures holding the input values")
  (Actor "InputParameters") (Label "inParams")

-- Variable for an object of the Constants class.
consts :: QuantityDict
consts = implVar "consts" (nounPhrase 
  "structure holding the constant values" 
  "structures holding the constant values") 
  (Actor "Constants") (Label "consts")