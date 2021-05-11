module Drasil.DblPendulum.Concepts where

import Language.Drasil

import Data.Drasil.Domains (physics)


pendulum :: CI
pendulum = commonIdeaWithDict "pendulum" (pn "Pendulum") "Pendulum" [physics]

concepts :: [IdeaDict]
concepts = [nw rod] 
       
rod :: NamedChunk
rod = nc "rod" (cn' "rod")




