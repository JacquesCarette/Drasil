module Drasil.DblPendulum.Concepts where

import Language.Drasil
import Data.Drasil.Theories.Physics (newtonSLRRC)


concepts :: [IdeaDict]
concepts = nw newtonSLRRC : map nw [rod, horizontal, vertical] ++ map nw defs
       
rod, horizontal, vertical :: NamedChunk
rod = nc "rod" (cn' "rod")
horizontal = nc "horizontal" (cn "horizontal") 
vertical = nc "vertical" (cn "vertical") 
---

defs :: [ConceptChunk]
defs = [arcLen]

arcLen :: ConceptChunk
arcLen = dcc "arc length" (nounPhraseSP "arc length") "the distance between two points on a curve"




