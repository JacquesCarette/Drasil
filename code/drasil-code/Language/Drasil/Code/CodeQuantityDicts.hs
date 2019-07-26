module Language.Drasil.Code.CodeQuantityDicts where

import Language.Drasil hiding (Space(..))
import Language.Drasil.Chunk.CodeQuantity (CodeQuantityDict, implCQD)
import Language.Drasil.Code.Code (CodeType(..))

inFileName :: CodeQuantityDict
inFileName = implCQD "inFileName" 
  (nounPhrase "name of the input file" "names of the input files") 
  Nothing String (Atomic "filename") Nothing