module Drasil.Projectile.Goals (goals) where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (goalStmtDom)

import Drasil.Projectile.Concepts (landingPos, projectile)

goals :: [ConceptInstance]
goals = [calcPosition]

calcPosition :: ConceptInstance
calcPosition = cic "calcPosition" 
  (S "Calculate" +:+. (phrase landingPos `ofThe` phrase projectile))
  "calcLandingPosition" goalStmtDom