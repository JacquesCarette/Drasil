-- | Defines concepts used in computing, to be used internally.
module Drasil.Metadata.Concepts.Computation (algorithm) where

import Language.Drasil (dcc, cn', ConceptChunk)

algorithm :: ConceptChunk
algorithm = dcc "algorithm" (cn' "algorithm")
  "a series of steps to be followed in calculations and problem-solving operations"
