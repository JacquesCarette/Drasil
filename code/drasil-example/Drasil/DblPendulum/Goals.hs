module Drasil.DblPendulum.Goals (goals, goalsInputs) where

import Language.Drasil
import Utils.Drasil
import Data.Drasil.Concepts.Documentation (goalStmtDom)
import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (mass, len)
import Data.Drasil.Concepts.Physics (gravitationalConst)
import Data.Drasil.Concepts.Math (iAngle)


goals :: [ConceptInstance]
goals = [motionMass]


goalsInputs :: [Sentence]
goalsInputs = [phrase CPP.mass +:+ phrase CPP.len  `ofThe` (S "rod" `sC` phrase iAngle ) `ofThe` phrase CPP.mass `andThe` phrase gravitationalConst ]

motionMass :: ConceptInstance
motionMass = cic "motionMass" 
  (S "Calculate the motion" `ofThe` phrase CPP.mass)
  "Motion-of-the-mass" goalStmtDom


