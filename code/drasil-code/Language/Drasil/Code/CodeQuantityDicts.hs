module Language.Drasil.Code.CodeQuantityDicts where

import Language.Drasil
import Language.Drasil.Chunk.CodeQuantity (CodeQuantityDict, implCQD)

inFileName :: CodeQuantityDict
inFileName = implCQD "inFileName" 
  (nounPhrase "name of the input file" "names of the input files") 
  Nothing String (Label "filename") Nothing

inParams :: CodeQuantityDict
inParams = implCQD "inParams" (nounPhrase 
  "structure holding the input values" "structures holding the input values")
  Nothing (Actor "InputParameters") (Label "inParams") Nothing

consts :: CodeQuantityDict
consts = implCQD "consts" (nounPhrase 
  "structure holding the constant values" 
  "structures holding the constant values") 
  Nothing (Actor "Constants") (Label "consts") Nothing