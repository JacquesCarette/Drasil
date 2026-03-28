module Drasil.Metadata.Concepts.Math (equation, graph, parameter, unit_) where

import Language.Drasil (dcc, cn', ConceptChunk)

equation, graph, parameter, unit_ :: ConceptChunk
equation     = dcc "equation"    (cn' "equation")               "A statement that the values of two mathematical expressions are equal "
graph        = dcc "graph"       (cn' "graph")                  "A diagram showing the relation between variable quantities"
parameter   = dcc "parameter"    (cn' "parameter")              "a quantity whose value is selected depending on particular circumstances"
unit_        = dcc "unit"        (cn' "unit")                   "Identity element"
