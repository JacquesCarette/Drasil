module Data.Drasil.Concepts.Computation where

import Language.Drasil
import Utils.Drasil.Concepts

import Data.Drasil.Concepts.Documentation (datum, input_, literacy, output_, 
  quantity, type_, value, variable)
import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.Domains (compScience)

algorithm, absTolerance, relTolerance:: ConceptChunk
algorithm = dcc "algorithm" (cn' "algorithm")
  "a series of steps to be followed in calculations and problem-solving operations"
absTolerance = dcc "absTolerance"   (cn' "Absolute tolerance") "a fixed number that is used to make direct comparisons"
relTolerance = dcc "relTolerance"   (cn' "Relative tolerance") " maximum amount of error that the user is willing to allow in the solution"

modCalcDesc :: Sentence -> ConceptChunk
modCalcDesc = dccWDS "modCalcDesc" (cn' "calculation")

compcon :: [NamedChunk]
compcon = [application, computer, structure, dataStruct, dataStruct', dataType, dataType', 
  inDatum, outDatum, inParam, inVar, inValue, inQty, computerLiteracy, computerApp]

application, computer, structure :: NamedChunk
os :: CI
-------------------------------------------------------------------------------
--  NC      |     |      id       |       term               |  abbreviation
-------------------------------------------------------------------------------
application = nc   "application"      (cn' "application") 
computer    = nc   "computer"         (cn' "computer") 
structure   = nc   "structure"        (cn' "structure")         
os          = commonIdeaWithDict "os" (cn' "operating system")    "OS"   [compScience]


dataStruct, dataStruct', dataType, dataType', 
  inDatum, outDatum, inParam, inVar, inValue, inQty,
  computerLiteracy, computerApp :: NamedChunk

dataStruct       = compoundNCPSPP datum structure
dataStruct'      = compoundNCPS datum structure
dataType         = compoundNCPSPP datum type_
dataType'        = compoundNCPS datum type_
inDatum          = compoundNC input_ datum
outDatum         = compoundNC output_ datum
inParam          = compoundNC input_ parameter
inVar            = compoundNC input_ variable
inValue          = compoundNC input_ value
inQty            = compoundNC input_ quantity
computerLiteracy = compoundNC computer literacy
computerApp      = compoundNC computer application
