module Drasil.DblPendulum.Concepts where

import Language.Drasil
import Data.Drasil.Theories.Physics (newtonSLRRC)

--below imports needed compoundNC part to work
import Utils.Drasil
import Data.Drasil.Concepts.Physics (pendulum, motion, position, velocity, force)


concepts :: [IdeaDict]
concepts = nw newtonSLRRC : map nw [rod, horizontal, vertical] 
  ++ map nw defs ++ map nw [compoundNC pendulum motion, 
  compoundNC horizontal position, compoundNC vertical position,
  compoundNC horizontal velocity, compoundNC vertical velocity,
  compoundNC horizontal force, compoundNC vertical force] 
  --The compoundNC stuff is used in various files and should probably be put into ChunkDB from there
       
rod, horizontal, vertical :: NamedChunk
rod = nc "rod" (cn' "rod")
horizontal = nc "horizontal" (cn "horizontal") 
vertical = nc "vertical" (cn "vertical") 
---

defs :: [ConceptChunk]
defs = [arcLen]

arcLen :: ConceptChunk
arcLen = dcc "arc length" (nounPhraseSP "arc length") "the distance between two points on a curve"




