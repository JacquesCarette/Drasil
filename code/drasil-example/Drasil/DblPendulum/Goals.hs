module Drasil.DblPendulum.Goals (goals, goalsInputs) where

import Language.Drasil
import Utils.Drasil
import Data.Drasil.Concepts.Documentation (goalStmtDom)
import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (mass, len)
import Data.Drasil.Concepts.Physics (gravitationalConst, motion)
import Data.Drasil.Concepts.Math (iAngle)
import Drasil.DblPendulum.Concepts (rod)


goals :: [ConceptInstance]
goals = [motionMass]


goalsInputs :: [Sentence]
goalsInputs = [(phrase CPP.mass `sAnd` phrase CPP.len  `the_ofThe` phrase rod) `sC` phrase iAngle `ofThe` phrase CPP.mass `andThe` phrase gravitationalConst ]

motionMass :: ConceptInstance
motionMass = cic "motionMass" 
  (S "Calculate" +:+ (phrase motion `the_ofThe` phrase CPP.mass))
  "Motion-of-the-mass" goalStmtDom


