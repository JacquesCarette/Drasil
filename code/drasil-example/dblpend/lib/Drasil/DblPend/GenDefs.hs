{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPend.GenDefs (genDefns, mvVelGD_1, mvVelGD_2, 
         mvAccelGD_1, mvAccelGD_2, mvForceGD_1, mvForceGD_2) where

import Prelude hiding (cos, sin, sqrt)

import Language.Drasil (ModelQDef, Derivation, Sentence(..), mkDerivName, eS, nounPhraseSP,
  (+:+), phrase, getUnit, mkQuantDef', sy, ($=))
import Utils.Drasil (weave)
import Theory.Drasil (GenDefn, equationalModel', gdNoRefs)
import Data.Drasil.Quantities.Physics (velocity, acceleration, force)
import qualified Drasil.DblPend.Expressions as E
import Drasil.DblPend.Unitals (mvVel_1, mvVel_2, mvAccel_1, mvAccel_2, mvForce_1, mvForce_2)

genDefns :: [GenDefn]
genDefns = [mvVelGD_1, mvVelGD_2, mvAccelGD_1, mvAccelGD_2, mvForceGD_1, mvForceGD_2]

-----------------------------------------------
-- Velocity Vector for First Object         --
-----------------------------------------------
mvVelGD_1 :: GenDefn
mvVelGD_1 = gdNoRefs (equationalModel' mvVelQD_1) (getUnit velocity) (Just mvVelDeriv_1) "velocityVector1" []

mvVelQD_1 :: ModelQDef  
mvVelQD_1 = mkQuantDef' mvVel_1 (nounPhraseSP "velocity vector of the first object") E.mvVelExpr_1

mvVelDeriv_1 :: Derivation
mvVelDeriv_1 = mkDerivName (phrase velocity +:+ S "of the first object") (weave [mvVelDerivSents_1, mvVelDerivEqns_1])

mvVelDerivSents_1 :: [Sentence]
mvVelDerivSents_1 = [S "The velocity vector represents both magnitude and direction in a unified representation.",
                     S "The velocity is expressed as a 2D vector with components in the horizontal and vertical directions.",
                     S "The angular velocity ω₁ and rod length L₁ are scaled with the perpendicular direction vector to form the complete velocity vector.",
                     S "This vector representation naturally preserves the rotational relationships in the pendulum system."]

mvVelDerivEqns_1 :: [Sentence]  
mvVelDerivEqns_1 = [eS $ sy mvVel_1 $= E.mvVelExpr_1,
                    S "where the velocity vector is obtained by scaling the perpendicular direction vector by the product of angular velocity and rod length"]

-----------------------------------------------
-- Velocity Vector for Second Object        --
-----------------------------------------------
mvVelGD_2 :: GenDefn
mvVelGD_2 = gdNoRefs (equationalModel' mvVelQD_2) (getUnit velocity) (Just mvVelDeriv_2) "velocityVector2" []

mvVelQD_2 :: ModelQDef
mvVelQD_2 = mkQuantDef' mvVel_2 (nounPhraseSP "velocity vector of the second object") E.mvVelExpr_2

mvVelDeriv_2 :: Derivation
mvVelDeriv_2 = mkDerivName (phrase velocity +:+ S "of the second object") (weave [mvVelDerivSents_2, mvVelDerivEqns_2])

mvVelDerivSents_2 :: [Sentence]
mvVelDerivSents_2 = [S "The velocity vector for the second object combines the velocity from the first pendulum with its own rotational motion.",
                     S "The total velocity is expressed as the vector sum of these two velocity contributions."]

mvVelDerivEqns_2 :: [Sentence]
mvVelDerivEqns_2 = [eS $ sy mvVel_2 $= E.mvVelExpr_2,
                    S "This represents the vector sum of the first pendulum's velocity and the second pendulum's relative velocity"]

-----------------------------------------------
-- Acceleration Vector for First Object     --
-----------------------------------------------
mvAccelGD_1 :: GenDefn
mvAccelGD_1 = gdNoRefs (equationalModel' mvAccelQD_1) (getUnit acceleration) (Just mvAccelDeriv_1) "accelerationVector1" []

mvAccelQD_1 :: ModelQDef
mvAccelQD_1 = mkQuantDef' mvAccel_1 (nounPhraseSP "acceleration vector of the first object") E.mvAccelExpr_1

mvAccelDeriv_1 :: Derivation
mvAccelDeriv_1 = mkDerivName (phrase acceleration +:+ S "of the first object") (weave [mvAccelDerivSents_1, mvAccelDerivEqns_1])

mvAccelDerivSents_1 :: [Sentence]
mvAccelDerivSents_1 = [S "The acceleration vector combines centripetal and tangential acceleration components.",
                       S "The centripetal acceleration points radially inward, while tangential acceleration is perpendicular to the rod.",
                       S "Both components are expressed as vectors and added using vector addition.",
                       S "Centripetal component: -ω₁²L₁ × direction_vector (radial inward)",
                       S "Tangential component: α₁L₁ × perpendicular_direction_vector (tangential)",
                       S "The vector sum preserves the underlying geometric relationships."]

mvAccelDerivEqns_1 :: [Sentence]
mvAccelDerivEqns_1 = [eS $ sy mvAccel_1 $= E.mvAccelExpr_1,
                      S "where the acceleration is the vector sum of centripetal and tangential components",
                      S "= centripetal_vector + tangential_vector"]

-----------------------------------------------
-- Acceleration Vector for Second Object    --
-----------------------------------------------
mvAccelGD_2 :: GenDefn
mvAccelGD_2 = gdNoRefs (equationalModel' mvAccelQD_2) (getUnit acceleration) (Just mvAccelDeriv_2) "accelerationVector2" []

mvAccelQD_2 :: ModelQDef
mvAccelQD_2 = mkQuantDef' mvAccel_2 (nounPhraseSP "acceleration vector of the second object") E.mvAccelExpr_2

mvAccelDeriv_2 :: Derivation
mvAccelDeriv_2 = mkDerivName (phrase acceleration +:+ S "of the second object") (weave [mvAccelDerivSents_2, mvAccelDerivEqns_2])

mvAccelDerivSents_2 :: [Sentence]
mvAccelDerivSents_2 = [S "The second object's acceleration is the vector sum of the first object's acceleration and its own relative acceleration.",
                       S "This captures the coupling between the two pendulums through their mechanical connection."]

mvAccelDerivEqns_2 :: [Sentence]
mvAccelDerivEqns_2 = [eS $ sy mvAccel_2 $= E.mvAccelExpr_2,
                      S "This represents the total acceleration as a vector sum"]

-----------------------------------------------
-- Force Vector for First Object            --
-----------------------------------------------
mvForceGD_1 :: GenDefn
mvForceGD_1 = gdNoRefs (equationalModel' mvForceQD_1) (getUnit force) (Just mvForceDeriv_1) "forceVector1" []

mvForceQD_1 :: ModelQDef
mvForceQD_1 = mkQuantDef' mvForce_1 (nounPhraseSP "force vector on the first object") E.mvForceExpr_1

mvForceDeriv_1 :: Derivation
mvForceDeriv_1 = mkDerivName (phrase force +:+ S "on the first object") (weave [mvForceDerivSents_1, mvForceDerivEqns_1])

mvForceDerivSents_1 :: [Sentence]
mvForceDerivSents_1 = [S "The force vector combines tension forces and gravitational force as vectors.",
                       S "The net force is obtained using vector addition, consistent with Newton's second law.",
                       S "Gravitational force acts vertically downward and is represented as a vector."]

mvForceDerivEqns_1 :: [Sentence]
mvForceDerivEqns_1 = [eS $ sy mvForce_1 $= E.mvForceExpr_1,
                      S "where the net force is the vector sum of tension forces and gravitational force"]

-----------------------------------------------
-- Force Vector for Second Object           --
-----------------------------------------------
mvForceGD_2 :: GenDefn
mvForceGD_2 = gdNoRefs (equationalModel' mvForceQD_2) (getUnit force) (Just mvForceDeriv_2) "forceVector2" []

mvForceQD_2 :: ModelQDef
mvForceQD_2 = mkQuantDef' mvForce_2 (nounPhraseSP "force vector on the second object") E.mvForceExpr_2

mvForceDeriv_2 :: Derivation
mvForceDeriv_2 = mkDerivName (phrase force +:+ S "on the second object") (weave [mvForceDerivSents_2, mvForceDerivEqns_2])

mvForceDerivSents_2 :: [Sentence]
mvForceDerivSents_2 = [S "The force on the second object combines tension from the second rod with gravitational effects.",
                       S "The vector representation maintains consistency between force and acceleration.",
                       S "This approach naturally handles the coupling forces between the connected pendulum objects."]

mvForceDerivEqns_2 :: [Sentence]
mvForceDerivEqns_2 = [eS $ sy mvForce_2 $= E.mvForceExpr_2,
                      S "This represents the net force as a vector sum",
                      S "= tension_vector + gravitational_force_vector"]