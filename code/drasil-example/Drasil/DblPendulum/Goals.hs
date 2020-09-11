module Drasil.DblPendulum.Goals (goals, goalsInputs) where

import Language.Drasil
import Utils.Drasil
import Data.Drasil.Concepts.Documentation (goalStmtDom)
import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (mass, len)
import Data.Drasil.Concepts.Physics (gravitationalConst)
import Data.Drasil.Concepts.Math (iAngle)
import Drasil.DblPendulum.Concepts

goals :: [ConceptInstance]
goals = [motionMass]

motionMass :: ConceptInstance
motionMass = cic "motionMass" 
  (S "Calculate the motion" `ofThe` phrase CPP.mass)
  "motionMass" goalStmtDom

goalsInputs :: [Sentence]
goalsInputs = [phrase CPP.mass +:+ phrase CPP.len  `ofThe` (S "rod" +:+ phrase iAngle ) `ofThe` phrase CPP.mass `andThe` phrase gravitationalConst ]


