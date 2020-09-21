module Drasil.DblPendulum.Concepts where

import Language.Drasil
--import Utils.Drasil

import Data.Drasil.IdeaDicts (physics)

--import Data.Drasil.Concepts.Math (angle)


{-concepts :: [IdeaDict]
concepts = map nw [landingPos, launch, launchAngle, launchSpeed, offset, targetPos]
  ++ map nw defs
-}
-- defs :: [ConceptChunk]
-- defs = [pendulum]


pendulumTitle :: CI
pendulumTitle = commonIdeaWithDict "pendulumTitle" (pn "Pendulum") "Pendulum" [physics]

-- duration, flightDur, landingPos, launch, launchAngle, launchSpeed, offset, targetPos :: NamedChunk
-- duration   = nc "duration" (nounPhraseSP "duration")
-- launch     = nc "launch"   (nounPhraseSP "launch") -- FIXME: Used as adjective


-- flightDur   = compoundNC (nc "flight"  (nounPhraseSP "flight" )) duration
-- landingPos  = compoundNC (nc "landing" (nounPhraseSP "landing")) position
-- launchAngle = compoundNC launch angle
-- launchSpeed = compoundNC launch speed
-- targetPos   = compoundNC target position-}






