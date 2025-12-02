-- | Defines concepts used in computing.
module Data.Drasil.Concepts.Computation where

import Language.Drasil (dcc, nc, cn', commonIdeaWithDict, Sentence,
  ConceptChunk, CI, IdeaDict, dccWDS)
import Language.Drasil.Chunk.Concept.NamedCombinators

import Data.Drasil.Concepts.Documentation (datum, input_, literacy, output_,
  quantity, type_, value, variable)
import Data.Drasil.Concepts.Math (parameter)
import Drasil.Metadata (compScience)

algorithm, absTolerance, relTolerance:: ConceptChunk
algorithm = dcc "algorithm" (cn' "algorithm")
  "a series of steps to be followed in calculations and problem-solving operations"
absTolerance = dcc "absTolerance"   (cn' "Absolute tolerance") "a fixed number that is used to make direct comparisons"
relTolerance = dcc "relTolerance"   (cn' "Relative tolerance") " maximum amount of error that the user is willing to allow in the solution"

modCalcDesc :: Sentence -> ConceptChunk
modCalcDesc = dccWDS "modCalcDesc" (cn' "calculation")

-- | Collects all computing-related named chunks (not concept-level yet).
compcon :: [IdeaDict]
compcon = [application, computer, structure, dataStruct, dataType,
  inDatum, outDatum, inParam, inVar, inValue, inQty, computerLiteracy, computerApp]

application, computer, structure :: IdeaDict
os :: CI
------------------------------------------------------------------------------------
--  NC      |     |      id       |       term             |  abbreviation | domain
-------------------------------------------------------------------------------------s
application = nc   "application"      (cn' "application")
computer    = nc   "computer"         (cn' "computer")
structure   = nc   "structure"        (cn' "structure")
os          = commonIdeaWithDict "os" (cn' "operating system")    "OS"   [compScience]

dataStruct, dataType, inDatum, outDatum, inParam, inVar, inValue, inQty,
  computerLiteracy, computerApp :: IdeaDict

dataStruct       = compoundNCPSPP datum structure
dataType         = compoundNCPSPP datum type_
inDatum          = compoundNC input_ datum
outDatum         = compoundNC output_ datum
inParam          = compoundNC input_ parameter
inVar            = compoundNC input_ variable
inValue          = compoundNC input_ value
inQty            = compoundNC input_ quantity
computerLiteracy = compoundNC computer literacy
computerApp      = compoundNC computer application
