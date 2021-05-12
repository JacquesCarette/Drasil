module Drasil.DblPendulum.Concepts where

import Language.Drasil

import Data.Drasil.Domains (physics)


pendulum :: CI
pendulum = commonIdeaWithDict "pendulum" (pn "Pendulum") "Pendulum" [physics]

concepts :: [IdeaDict]
concepts = map nw [rod, horizontal, vertical] ++ map nw defs 
       
rod, horizontal, vertical :: NamedChunk
rod = nc "rod" (cn' "rod")
horizontal = nc "horizontal" (cn "horizontal") 
vertical = nc "vertical" (cn "vertical") 
---

defs:: [ConceptChunk]
defs = [shm, amplitude, component, arcLen, motion]

shm, amplitude, component, arcLen, motion :: ConceptChunk
shm = dcc "SHM" (nounPhraseSP "simple harmonic motion") ("repetitive movement back and forth through" ++
                                                         "an equilibrium position such that the maximum" ++
                                                         "displacement on one side is equal to the maximum" ++
                                                         "displacement on the other side")
amplitude = dcc "amplitude" (nounPhraseSP "amplitude") ("the maximum displacement of a body measured from" ++
                                                        "its equilibrium position")
component = dcc "component" (nounPhrase "component" "components") "one part or direction of a vector"
arcLen = dcc "arc length" (nounPhraseSP "arc length") "the distance between two points on a curve"
motion = dcc "motion" (nounPhraseSP "motion") "the act of changing position from one place to another"




