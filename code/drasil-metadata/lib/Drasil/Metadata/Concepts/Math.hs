module Drasil.Metadata.Concepts.Math (graph, unit_) where

import Language.Drasil (dcc, cn', ConceptChunk)

graph, unit_ :: ConceptChunk
graph        = dcc "graph"        (cn' "graph")                   "A diagram showing the relation between variable quantities"
unit_        = dcc "unit"         (cn' "unit")                   "Identity element"
