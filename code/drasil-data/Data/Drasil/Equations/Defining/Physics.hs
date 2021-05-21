module Data.Drasil.Equations.Defining.Physics where

-- We define both some basic equations of physics, and their wrappers as concepts
--
import Language.Drasil
import Utils.Drasil (foldlSent, getTandS)
import Utils.Drasil.Sentence (sIs, sOf, the_ofThe)

import qualified Data.Drasil.Quantities.Math as QM (unitVectj)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, time,
  force, gravitationalAccel, height, pressure, weight, velocity, position)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (density, 
  mass, specWeight, vol)
import Data.Drasil.Concepts.Documentation (body, constant)

------------------------------------------------------------------------------------------------------
-- The equations

newtonSLRel, weightEqn, weightDerivAccelEqn, weightDerivNewtonEqn, weightDerivReplaceMassEqn,
  weightDerivSpecWeightEqn,
  hsPressureEqn, accelerationEqn, velocityEqn  :: Relation

newtonSLRel               = sy QP.force $= sy QPP.mass * sy QP.acceleration

weightEqn                 = sy QP.weight $= sy QPP.vol * sy QPP.specWeight
weightDerivNewtonEqn      = sy QP.weight $= sy QPP.mass * sy QP.gravitationalAccel
weightDerivReplaceMassEqn = sy QP.weight $= sy QPP.density * sy QPP.vol * sy QP.gravitationalAccel
weightDerivSpecWeightEqn  = sy QP.weight $= sy QPP.vol * sy QPP.specWeight

weightDerivAccelEqn       = sy QP.acceleration $= vec2D 0 (sy QP.gravitationalAccel * sy QM.unitVectj)

hsPressureEqn             = sy QP.pressure $= sy QPP.specWeight * sy QP.height

velocityEqn               = sy QP.velocity $= deriv (sy QP.position) QP.time
accelerationEqn           = sy QP.acceleration $= deriv (sy QP.velocity) QP.time

------------------------------------------------------------------------------------------------------
-- The concepts

accelerationRC, velocityRC :: RelationConcept

accelerationRC = addRelToCC QP.acceleration "accelerationRC" accelerationEqn
velocityRC = addRelToCC QP.velocity "velocityRC" velocityEqn

newtonSLRC :: RelationConcept
newtonSLRC = makeRC "newtonSL" (nounPhraseSP "Newton's second law of motion")
  newtonSLDesc newtonSLRel

newtonSLDesc :: Sentence
newtonSLDesc = foldlSent [S "The net", getTandS QP.force, S "on a",
  phrase body `sIs` S "proportional to", getTandS QP.acceleration `the_ofThe`
  phrase body `sC` S "where", ch QPP.mass, S "denotes", phrase QPP.mass `the_ofThe`
  phrase body, S "as the", phrase constant `sOf` S "proportionality"]


