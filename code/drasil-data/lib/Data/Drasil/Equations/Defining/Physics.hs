-- | Defines some basic equations of physics and their wrappers as concepts.
module Data.Drasil.Equations.Defining.Physics where

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S (is, of_, the_ofThe)

import qualified Data.Drasil.Quantities.Physics as QP (acceleration, time,
  force, height, velocity, position)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass, specWeight, vol)
import Data.Drasil.Concepts.Documentation (body, constant)

------------------------------------------------------------------------------------------------------
-- * Equations

weightEqn, newtonSLEqn, hsPressureEqn, speedEqn :: ExprC r => r
newtonSLEqn               = sy QPP.mass $* sy QP.acceleration
weightEqn                 = sy QPP.vol $* sy QPP.specWeight
hsPressureEqn             = sy QPP.specWeight $* sy QP.height
speedEqn                  = norm (sy QP.velocity)

velocityEqn, accelerationEqn :: ModelExpr
velocityEqn               = deriv (sy QP.position) QP.time
accelerationEqn           = deriv (sy QP.velocity) QP.time

------------------------------------------------------------------------------------------------------
-- * Concepts

accelerationQD :: ModelQDef
accelerationQD = mkQuantDef QP.acceleration accelerationEqn

velocityQD :: ModelQDef
velocityQD = mkQuantDef QP.velocity velocityEqn

newtonSLQD :: ModelQDef
newtonSLQD = fromEqn' "force" (nounPhraseSP "Newton's second law of motion")
  newtonSLDesc (eqSymb QP.force) Real newtonSLEqn

newtonSLDesc :: Sentence
newtonSLDesc = foldlSent [S "The net", getTandS QP.force, S "on a",
  phrase body `S.is` S "proportional to", getTandS QP.acceleration `S.the_ofThe`
  phrase body `sC` S "where", ch QPP.mass, S "denotes", phrase QPP.mass `S.the_ofThe`
  phrase body, S "as the", phrase constant `S.of_` S "proportionality"]
