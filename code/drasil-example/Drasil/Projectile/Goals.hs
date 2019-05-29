module Drasil.Projectile.Goals (goals) where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (goalStmtDom)
import Data.Drasil.Concepts.Physics (position)

goals :: [ConceptInstance]
goals = [calcPosition]

calcPosition :: ConceptInstance
calcPosition = cic "calcPosition" 
  (S "Calculate" +:+. ((S "landing" +:+ phrase position) `ofThe` S "projectile"))
  "calcLandingPosition" goalStmtDom