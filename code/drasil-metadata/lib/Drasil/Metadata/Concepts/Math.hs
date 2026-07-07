module Drasil.Metadata.Concepts.Math (equation, graph, parameter, unit_) where

import Language.Drasil (cn', ConceptChunk, Sentence(..), cncpt''')
import Drasil.Database (mkUid)

equation, graph, parameter, unit_ :: ConceptChunk
equation     = cncpt''' (mkUid "equation")  (cn' "equation")
  (S "A statement that the values of two mathematical expressions are equal ")
graph        = cncpt''' (mkUid "graph")     (cn' "graph")
  (S "A diagram showing the relation between variable quantities")
parameter    = cncpt''' (mkUid "parameter") (cn' "parameter")
  (S "a quantity whose value is selected depending on particular circumstances")
unit_        = cncpt''' (mkUid "unit")      (cn' "unit") (S "Identity element")
