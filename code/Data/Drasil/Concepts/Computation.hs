module Data.Drasil.Concepts.Computation where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (datum, input_, literacy, output_, 
    quantity, type_, value, variable)
import qualified Language.Drasil.NounPhrase as NP (phrase, plural)
import Data.Drasil.Concepts.Math (parameter)


algorithm :: ConceptChunk
algorithm    = dcc "algorithm" (cn' "algorithm")
  "A series of steps to be followed in calculations and problem-solving operations"

mod_calc_desc :: Sentence -> ConceptChunk
mod_calc_desc defnFromEx = dccWDS "mod_calc_desc" (cn' "calculation") defnFromEx

application, computer, structure :: NamedChunk
os :: CI
-------------------------------------------------------------------------------
--  NC      |     |      id       |       term               |  abbreviation
-------------------------------------------------------------------------------
application  = nc  "application"    (cn' "application")      
computer     = nc  "computer"       (cn' "computer")         
structure    = nc  "structure"      (cn' "structure")         
os           = commonIdea "os"      (cn' "operating system")    "OS"


dataStruct, dataStruct', dataType, dataType', 
  inDatum, outDatum, inParam, inVar, inValue, inQty,
  computerLiteracy, computerApp :: NamedChunk

dataStruct       = compoundNC'' NP.plural NP.phrase datum structure
dataStruct'      = compoundNC'' NP.plural NP.plural datum structure
dataType         = compoundNC'' NP.plural NP.phrase datum type_
dataType'        = compoundNC'' NP.plural NP.plural datum type_
inDatum          = compoundNC input_ datum
outDatum         = compoundNC output_ datum
inParam          = compoundNC input_ parameter
inVar            = compoundNC input_ variable
inValue          = compoundNC input_ value
inQty            = compoundNC input_ quantity
computerLiteracy = compoundNC computer literacy
computerApp      = compoundNC computer application
