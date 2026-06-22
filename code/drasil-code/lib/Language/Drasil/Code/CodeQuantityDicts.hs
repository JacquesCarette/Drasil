-- | Defines QuantityDicts needed in the code generation stage.
module Language.Drasil.Code.CodeQuantityDicts (
  inFileName, inParams, consts, codeDQDs
) where

import Drasil.Database (mkUid)
import Language.Drasil

-- | Variable for the input file's name.
inFileName :: DefinedQuantityDict
inFileName = implVar' (mkUid "inFileName")
  (nounPhrase "name of the input file" "names of the input files")
  (S "a filepath, absolute or relative, to the file containing the program's inputs")
  String (label "filename")

-- | Variable for an object of the InputParameters class.
inParams :: DefinedQuantityDict
inParams = implVar' (mkUid "inParams") (nounPhrase
  "structure holding the input values" "structures holding the input values")
  (S "the structure holding the input values")
  (Actor "InputParameters") (label "inParams")

-- | Variable for an object of the Constants class.
consts :: DefinedQuantityDict
consts = implVar' (mkUid "consts") (nounPhrase
  "structure holding the constant values"
  "structures holding the constant values")
  (S "the structure holding the constant values")
  (Actor "Constants") (label "consts")

codeDQDs :: [DefinedQuantityDict]
codeDQDs = [inFileName, inParams, consts]
