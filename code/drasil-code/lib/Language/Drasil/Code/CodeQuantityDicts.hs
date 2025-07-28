-- | Defines QuantityDicts needed in the code generation stage.
module Language.Drasil.Code.CodeQuantityDicts where

import Language.Drasil

-- | Variable for the input file's name.
inFileName :: DefinedQuantityDict
inFileName = implVar' "inFileName"
  (nounPhrase "name of the input file" "names of the input files")
  (S "the name of the input file")
  String (label "filename")

-- | Variable for an object of the InputParameters class.
inParams :: DefinedQuantityDict
inParams = implVar' "inParams" (nounPhrase
  "structure holding the input values" "structures holding the input values")
  (S "the structure holding the input values")
  (Actor "InputParameters") (label "inParams")

-- | Variable for an object of the Constants class.
consts :: DefinedQuantityDict
consts = implVar' "consts" (nounPhrase
  "structure holding the constant values"
  "structures holding the constant values")
  (S "the structure holding the constant values")
  (Actor "Constants") (label "consts")


codeDQDs :: [DefinedQuantityDict]
codeDQDs = [inFileName, inParams, consts]