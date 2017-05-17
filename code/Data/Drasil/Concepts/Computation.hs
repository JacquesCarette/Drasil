module Data.Drasil.Concepts.Computation where

import Language.Drasil
import Control.Lens((^.))
import Data.Drasil.Concepts.Documentation


algorithm :: ConceptChunk
algorithm    = dcc "algorithm"    (cn' "algorithm")               "A series of steps to be followed in calculations and problem-solving operations"



computer, os, structure, type_ :: NPNC

computer        = npnc "computer"       (cn' "computer")
os              = npnc' "os"            (cn' "operating system") "OS"
structure       = npnc "structure"      (cn' "structure")
type_           = npnc "type"           (cn' "type")


dataStruct, dataStruct', dataType, dataType', inDatum, outDatum ::NPNC

dataStruct                   = compoundNPNC'' plural phrase datum structure
dataStruct'                  = compoundNPNC'' plural plural datum structure
dataType                     = compoundNPNC'' plural phrase datum type_
dataType'                    = compoundNPNC'' plural plural datum type_
inDatum                      = compoundNPNC input_ datum
outDatum                     = compoundNPNC output_ datum