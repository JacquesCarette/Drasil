-- | Defines QuantityDicts needed in the code generation stage.
module Language.Drasil.Code.CodeQuantityDicts where

import Language.Drasil

-- | Variable for the input file's name.
inFileName :: DefinedQuantityDict
inFileName = dqdNoUnit (dccWDS "inFileName"
  (nounPhrase "name of the input file" "names of the input files")
  (S "the name of the input file"))
  (label "filename") String

-- | Variable for an object of the InputParameters class.
inParams :: DefinedQuantityDict
inParams = dqdNoUnit (dccWDS "inParams" (nounPhrase
  "structure holding the input values" "structures holding the input values")
  (S "the structure holding the input values"))
  (label "inParams") (Actor "InputParameters")

-- | Variable for an object of the Constants class.
consts :: DefinedQuantityDict
consts = dqdNoUnit (dccWDS "consts" (nounPhrase
  "structure holding the constant values"
  "structures holding the constant values")
  (S "the structure holding the constant values"))
  (label "consts") (Actor "Constants")
