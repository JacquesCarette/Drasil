module Drasil.DblPendulum.Concepts where

import Language.Drasil


concepts :: [IdeaDict]
concepts = map nw [rod, horizontal, vertical] ++ map nw defs 
       
rod, horizontal, vertical :: NamedChunk
rod = nc "rod" (cn' "rod")
horizontal = nc "horizontal" (cn "horizontal") 
vertical = nc "vertical" (cn "vertical") 
---

defs :: [ConceptChunk]
defs = [arcLen]

arcLen :: ConceptChunk
arcLen = dcc "arc length" (nounPhraseSP "arc length") "the distance between two points on a curve"




