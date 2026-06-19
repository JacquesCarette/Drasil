-- | Defines concepts used in computing, to be used internally.
module Drasil.Metadata.Concepts.Computation (algorithm) where

import Language.Drasil (cn', ConceptChunk, cncpt''', Sentence(..))
import Drasil.Database (mkUid)

algorithm :: ConceptChunk
algorithm = cncpt''' (mkUid "algorithm") (cn' "algorithm")
  (S "a series of steps to be followed in calculations and problem-solving operations")
