module Drasil.DblPendulum.Concepts where

import Language.Drasil

import Data.Drasil.Domains (physics)


pendulum :: CI
pendulum = commonIdeaWithDict "pendulum" (pn "Pendulum") "Pendulum" [physics]

concepts :: [IdeaDict]
concepts = [nw rod] 
       
rod :: NamedChunk
rod = nc "rod" (cn' "rod")
horizontal = nc "horizontal" (cn "horizontal") 
vertical = nc "vertical" (cn "vertical") 

shm, amplitude, component, arcLen, motion :: ConceptChunk
shm = dcc "SHM" (nounPhraseSP "simple harmonic motion") ("repetitive movement back and forth through" ++
                                                         "an equilibrium position such that the maximum" ++
                                                         "displacement on one side is equal to the maximum" ++
                                                         "displacement on the other side")
amplitude = dcc "amplitude" (nounPhraseSP "amplitude") ("the maximum displacement of a body measured from" ++
                                                        "its equilibrium position")

component
arcLen
motion




