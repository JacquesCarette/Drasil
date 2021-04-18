module Drasil.Projectile.TMods (tMods, accelerationTM, velocityTM) where

import Language.Drasil
import Theory.Drasil (TheoryModel, tm)
import Data.Drasil.Quantities.Physics (acceleration, position, time, velocity)
import Data.Drasil.Equations.Defining.Physics (accelerationEqn, velocityEqn, accelerationRC,
  velocityRC)
import Data.Drasil.Citations (accelerationWiki, velocityWiki)

tMods :: [TheoryModel]
tMods = [accelerationTM, velocityTM]

accelerationTM :: TheoryModel
accelerationTM = tm (cw accelerationRC)
  [qw acceleration, qw velocity, qw time] ([] :: [ConceptChunk]) [] [accelerationEqn] []
  [makeCite accelerationWiki] "acceleration" []

----------

velocityTM :: TheoryModel
velocityTM = tm (cw velocityRC)
  [qw velocity, qw position, qw time] ([] :: [ConceptChunk]) [] [velocityEqn] []
  [makeCite velocityWiki] "velocity" []
