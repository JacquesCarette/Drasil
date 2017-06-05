module Data.Drasil.Concepts.Computation where

import Language.Drasil
import Data.Drasil.Concepts.Documentation


algorithm :: ConceptChunk
algorithm    = dcc "algorithm"    (cn' "algorithm")               "A series of steps to be followed in calculations and problem-solving operations"

mod_calc_desc :: Sentence -> ConceptChunk
mod_calc_desc defnFromEx = dccWDS "mod_calc_desc" (cn' "calculation")   defnFromEx


computer, os, structure, type_ :: NamedChunk

computer        = npnc "computer"       (cn' "computer")
os              = npnc' "os"            (cn' "operating system") "OS"
structure       = npnc "structure"      (cn' "structure")
type_           = npnc "type"           (cn' "type")


dataStruct, dataStruct', dataType, dataType', inDatum, outDatum :: NamedChunk

dataStruct                   = compoundNC'' plural phrase datum structure
dataStruct'                  = compoundNC'' plural plural datum structure
dataType                     = compoundNC'' plural phrase datum type_
dataType'                    = compoundNC'' plural plural datum type_
inDatum                      = compoundNC input_ datum
outDatum                     = compoundNC output_ datum