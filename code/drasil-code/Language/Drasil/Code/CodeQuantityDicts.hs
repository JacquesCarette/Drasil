module Language.Drasil.Code.CodeQuantityDicts where

import Language.Drasil

inFileName :: QuantityDict
inFileName = implVar "inFileName" 
  (nounPhrase "name of the input file" "names of the input files") String
  (Label "filename")

inParams :: QuantityDict
inParams = implVar "inParams" (nounPhrase 
  "structure holding the input values" "structures holding the input values")
  (Actor "InputParameters") (Label "inParams")

consts :: QuantityDict
consts = implVar "consts" (nounPhrase 
  "structure holding the constant values" 
  "structures holding the constant values") 
  (Actor "Constants") (Label "consts")