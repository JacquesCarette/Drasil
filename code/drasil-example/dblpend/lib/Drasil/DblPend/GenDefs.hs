{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPend.GenDefs (genDefns, mvVelGD_1, mvVelGD_2, 
         mvAccelGD_1, mvAccelGD_2, mvForceGD_1, mvForceGD_2) where

import Prelude hiding (cos, sin, sqrt)
import Language.Drasil
import Utils.Drasil (weave)
import Theory.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import qualified Language.Drasil.NounPhrase.Combinators as NP
import Data.Drasil.Quantities.Physics (velocity, acceleration, force, time)
import Drasil.DblPend.Unitals (lenRod_1, mvVel_1, mvVel_2,
    mvAccel_1, mvAccel_2, mvForce_1, mvForce_2,
    posVec_1, posVec_2, lenRod_2, pendDisAngle_1, pendDisAngle_2,
    angularVel_1, angularVel_2, tension_1, tension_2, massObj_1, massObj_2)
import qualified Data.Drasil.Quantities.Physics as QP (gravitationalAccel)
import Drasil.DblPend.DataDefs (positionVecDD_1)
import Data.Drasil.Concepts.Math (vector)
import qualified Drasil.DblPend.Expressions as E
import Drasil.DblPend.Concepts 

genDefns :: [GenDefn]
genDefns = [mvVelGD_1, mvVelGD_2, mvAccelGD_1, mvAccelGD_2, mvForceGD_1, mvForceGD_2]

-----------------------------------------------
-- Velocity Vector for First Object         --
-----------------------------------------------
mvVelGD_1 :: GenDefn
mvVelGD_1 = gdNoRefs (equationalModel' mvVelQD_1) (getUnit velocity) (Just mvVelDeriv_1) "velocityVector1" []

mvVelQD_1 :: ModelQDef
mvVelQD_1 = mkQuantDef' mvVel_1 (the vector `NP.of_` (velocity `ofThe` firstObject)) E.mvVelExpr_1
-- lable and equation

mvVelDeriv_1 :: Derivation
mvVelDeriv_1 = mkDerivName (phraseNP (NP.the (vector`of_` velocity))) (weave [mvVelDerivSents_1, mvVelDerivEqns_1])
-- title paragraph and weave the explained words and refined equation

-- Derivation step sentences (vector -> component -> vector form)
velDerivSent1, velXDerivSent2_1, velDerivSent3, velDerivSent4, velDerivSent5 :: Sentence
velDerivSent1 = S "At a given point in time" `sC` phrase velocity `S.is` definedIn'' positionVecDD_1
velXDerivSent2_1 = S "We also know the" +:+ phrase horizontalPos +:+ S "that" `S.is` definedIn'' positionVecDD_1
velDerivSent3 = S "Applying this,"
velDerivSent4 = eS' lenRod_1 `S.is` S "constant" `S.wrt` S "time, so"
velDerivSent5 = S "Therefore, using the chain rule,"
mvVelDerivSents_1 :: [Sentence]
mvVelDerivSents_1 = [velDerivSent1, velXDerivSent2_1, velDerivSent3, velDerivSent4, velDerivSent5]

mvVelDerivEqns_1 :: [Sentence]
mvVelDerivEqns_1 = [
    -- Definition: velocity is the time derivative of the position vector
    eS $ sy mvVel_1 $= deriv (sy posVec_1) time,
    -- Position vector definition (r1 = L1 * [sin θ1, -cos θ1])
    eS $ sy posVec_1 $= sy lenRod_1 $* E.vector E.sinAngleExpr1 (neg E.cosAngleExpr1),
    -- Take time derivative and apply the chain rule to each component
    eS $ deriv (sy posVec_1) time $= sy lenRod_1 $* E.vector (cos (sy pendDisAngle_1) $* sy angularVel_1)
                                                                                                                     (sin (sy pendDisAngle_1) $* sy angularVel_1),
    -- Factor out angular velocity and rod length to expose the perpendicular direction vector
    eS $ sy lenRod_1 $* E.vector (cos (sy pendDisAngle_1) $* sy angularVel_1)
                                 (sin (sy pendDisAngle_1) $* sy angularVel_1)
                                 $= (sy angularVel_1 $* sy lenRod_1) `cScale` E.perpDirectionVector_1,
    -- Final simplified velocity expression
    eS $ sy mvVel_1 $= E.mvVelExpr_1
    ]


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
mvVelDerivEqns_2 = [
    -- Definition: velocity of second bob is derivative of its position
    eS $ sy mvVel_2 $= deriv (sy posVec_2) time,
    -- Position r2 = r1 + L2 * [sin θ2, -cos θ2]
    eS $ sy posVec_2 $= sy posVec_1 $+ (sy lenRod_2 $* E.vector E.sinAngleExpr2 (neg E.cosAngleExpr2)),
    -- Differentiate: derivative of r2 = derivative of r1 + derivative of the second term
    eS $ deriv (sy posVec_2) time $= deriv (sy posVec_1) time $+ sy lenRod_2 $* E.vector (cos (sy pendDisAngle_2) $* sy angularVel_2)
                                                                                                                                                                                 (sin (sy pendDisAngle_2) $* sy angularVel_2),
    -- Recognize derivative of r1 is mvVel_1 and factor to show perpendicular scaling
    eS $ deriv (sy posVec_1) time $+ (sy angularVel_2 $* sy lenRod_2) `cScale` E.perpDirectionVector_2
             $= E.mvVelExpr_1 `cAdd` ((sy angularVel_2 $* sy lenRod_2) `cScale` E.perpDirectionVector_2),
    -- Final simplified velocity expression for the second object
    eS $ sy mvVel_2 $= E.mvVelExpr_2
    ]

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
mvAccelDerivEqns_1 = [
    -- Definition: acceleration is time derivative of velocity
    eS $ sy mvAccel_1 $= deriv (sy mvVel_1) time,
    -- Substitute velocity expression (ω1 L1) cScale perpDirectionVector_1
    eS $ sy mvVel_1 $= (sy angularVel_1 $* sy lenRod_1) `cScale` E.perpDirectionVector_1,
    -- Differentiate: gives centripetal and tangential components
    eS $ deriv (sy mvVel_1) time $= E.centripetalAccel_1 `cAdd` E.tangentialAccel_1,
    -- Final simplified acceleration expression
    eS $ sy mvAccel_1 $= E.mvAccelExpr_1
    ]

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
mvAccelDerivEqns_2 = [
    eS $ sy mvAccel_2 $= deriv (sy mvVel_2) time,
    -- mvVel_2 = mvVel_1 + component from second rod
    eS $ sy mvVel_2 $= E.mvVelExpr_1 `cAdd` E.mvVelComponent_2,
    -- Differentiate: derivative of mvVel_1 is mvAccel_1; derivative of mvVelComponent_2 gives centripetal+ tangential for 2
    eS $ deriv (sy mvVel_2) time $= deriv (sy mvVel_1) time `cAdd` (E.centripetalAccel_2 `cAdd` E.tangentialAccel_2),
    eS $ sy mvAccel_2 $= E.mvAccelExpr_1 `cAdd` (E.centripetalAccel_2 `cAdd` E.tangentialAccel_2),
    eS $ sy mvAccel_2 $= E.mvAccelExpr_2
    ]

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
mvForceDerivEqns_1 = [
    -- Start with sum of forces (symbolic, not yet vectorized)
        eS $ sy mvForce_1 $= sy tension_1 `cAdd` sy tension_2 `cAdd` (sy massObj_1 `cScale` sy QP.gravitationalAccel),
    -- Newton: F = m * a (vector form)
    eS $ sy mvForce_1 $= sy massObj_1 `cScale` sy mvAccel_1,
    -- Expand acceleration definition
    eS $ sy mvAccel_1 $= E.mvAccelExpr_1,
    -- Tension and gravity vector definitions (vector form)
    eS $ sy mvForce_1 $= negClif E.tensionVec_1 `cAdd` E.tensionVec_2 `cAdd` E.gravitationalForce_1,
    -- Final simplified force expression
    eS $ sy mvForce_1 $= E.mvForceExpr_1
    ]

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
mvForceDerivEqns_2 = [
    -- Start with sum of forces (symbolic, not yet vectorized)
    eS $ sy mvForce_2 $= sy tension_2 `cAdd` (sy massObj_2 `cScale` sy QP.gravitationalAccel),
    -- Newton: F = m * a (vector form)
    eS $ sy mvForce_2 $= sy massObj_2 `cScale` sy mvAccel_2,
    -- Expand acceleration definition
    eS $ sy mvAccel_2 $= E.mvAccelExpr_2,
    -- Tension and gravity vector definitions (vector form)
    eS $ sy mvForce_2 $= negClif E.tensionVec_2 `cAdd` E.gravitationalForce_2,
    -- Final simplified force expression
    eS $ sy mvForce_2 $= E.mvForceExpr_2
    ]