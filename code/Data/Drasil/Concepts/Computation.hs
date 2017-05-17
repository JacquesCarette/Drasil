module Data.Drasil.Concepts.Computation where

import Language.Drasil
import Control.Lens((^.))
import Data.Drasil.Concepts.Documentation

os, structure :: NPNC

os              = npnc' "os"            (cn' "operating system") "OS"
structure       = npnc "structure"      (cn' "structure")






dataStruct, dataStruct', dataType, dataType', inDatum, outDatum ::NPNC

dataStruct                   = compoundNPNC'' plural phrase datum structure
dataStruct'                  = compoundNPNC'' plural plural datum structure
dataType                     = compoundNPNC'' plural phrase datum type_
dataType'                    = compoundNPNC'' plural plural datum type_
inDatum                      = compoundNPNC input_ datum
outDatum                     = compoundNPNC output_ datum