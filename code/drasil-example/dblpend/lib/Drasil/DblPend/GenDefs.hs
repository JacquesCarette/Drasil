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
-- Multivector Velocity for First Object    --
-----------------------------------------------
mvVelGD_1 :: GenDefn
mvVelGD_1 = gdNoRefs (equationalModel' mvVelQD_1) (getUnit velocity) (Just mvVelDeriv_1) "multivectorVelocity1" []

mvVelQD_1 :: ModelQDef  
mvVelQD_1 = mkQuantDef' mvVel_1 (nounPhraseSP "multivector velocity of the first object") E.mvVelExpr_1

mvVelDeriv_1 :: Derivation
mvVelDeriv_1 = mkDerivName (phrase velocity +:+ S "of the first object") (weave [mvVelDerivSents_1, mvVelDerivEqns_1])

mvVelDerivSents_1 :: [Sentence]
mvVelDerivSents_1 = [S "The multivector velocity in Clifford algebra represents both magnitude and direction as a unified geometric entity.",
                     S "Using the 2D Clifford space Cl(2,0), the velocity is expressed as a vector multivector with components in the e₁ and e₂ basis directions.",
                     S "This approach provides a natural geometric representation that preserves rotational relationships.",
                     S "The angular velocity ω₁ and rod length L₁ combine with the direction vector to form the complete velocity multivector.",
                     S "Unlike traditional component-wise vector addition, this geometric product maintains the intrinsic geometric structure."]

mvVelDerivEqns_1 :: [Sentence]  
mvVelDerivEqns_1 = [eS $ sy mvVel_1 $= E.mvVelExpr_1,
                    S "where the velocity multivector combines angular velocity and rod length in Clifford space",
                    S "Direction vector = cos(θ₁)e₁ + sin(θ₁)e₂ using basis vectors e₁, e₂",
                    S "The geometric product preserves both magnitude and geometric orientation"]

-----------------------------------------------
-- Multivector Velocity for Second Object   --
-----------------------------------------------
mvVelGD_2 :: GenDefn
mvVelGD_2 = gdNoRefs (equationalModel' mvVelQD_2) (getUnit velocity) (Just mvVelDeriv_2) "multivectorVelocity2" []

mvVelQD_2 :: ModelQDef
mvVelQD_2 = mkQuantDef' mvVel_2 (nounPhraseSP "multivector velocity of the second object") E.mvVelExpr_2

mvVelDeriv_2 :: Derivation
mvVelDeriv_2 = mkDerivName (phrase velocity +:+ S "of the second object") (weave [mvVelDerivSents_2, mvVelDerivEqns_2])

mvVelDerivSents_2 :: [Sentence]
mvVelDerivSents_2 = [S "The multivector velocity for the second object combines the velocity from the first pendulum with its own rotational motion.",
                     S "In Clifford algebra, this vector addition preserves the geometric relationships between the two connected pendulums.",
                     S "The total velocity is expressed as the sum of two vector multivectors in the same geometric space."]

mvVelDerivEqns_2 :: [Sentence]
mvVelDerivEqns_2 = [eS $ sy mvVel_2 $= E.mvVelExpr_2,
                    S "This represents the geometric sum of the first pendulum's velocity and the second pendulum's relative velocity"]

-----------------------------------------------
-- Multivector Acceleration for First Object--
-----------------------------------------------
mvAccelGD_1 :: GenDefn
mvAccelGD_1 = gdNoRefs (equationalModel' mvAccelQD_1) (getUnit acceleration) (Just mvAccelDeriv_1) "multivectorAcceleration1" []

mvAccelQD_1 :: ModelQDef
mvAccelQD_1 = mkQuantDef' mvAccel_1 (nounPhraseSP "multivector acceleration of the first object") E.mvAccelExpr_1

mvAccelDeriv_1 :: Derivation
mvAccelDeriv_1 = mkDerivName (phrase acceleration +:+ S "of the first object") (weave [mvAccelDerivSents_1, mvAccelDerivEqns_1])

mvAccelDerivSents_1 :: [Sentence]
mvAccelDerivSents_1 = [S "The multivector acceleration in Clifford algebra combines centripetal and tangential acceleration components.",
                       S "The centripetal acceleration points radially inward, while tangential acceleration is perpendicular to the rod.",
                       S "Both components are naturally expressed as vector multivectors and added using Clifford geometric operations.",
                       S "Centripetal component: -ω₁²L₁ × direction_vector (radial inward)",
                       S "Tangential component: α₁L₁ × perpendicular_direction_vector (tangential)",
                       S "The geometric sum preserves the underlying geometric relationships without component-wise decomposition."]

mvAccelDerivEqns_1 :: [Sentence]
mvAccelDerivEqns_1 = [eS $ sy mvAccel_1 $= E.mvAccelExpr_1,
                      S "where the acceleration combines centripetal and tangential components in Clifford space",
                      S "= centripetal_multivector + tangential_multivector",
                      S "Both terms maintain their geometric significance through the Clifford algebra representation"]

-----------------------------------------------
-- Multivector Acceleration for Second Object--
-----------------------------------------------
mvAccelGD_2 :: GenDefn  
mvAccelGD_2 = gdNoRefs (equationalModel' mvAccelQD_2) (getUnit acceleration) (Just mvAccelDeriv_2) "multivectorAcceleration2" []

mvAccelQD_2 :: ModelQDef
mvAccelQD_2 = mkQuantDef' mvAccel_2 (nounPhraseSP "multivector acceleration of the second object") E.mvAccelExpr_2

mvAccelDeriv_2 :: Derivation
mvAccelDeriv_2 = mkDerivName (phrase acceleration +:+ S "of the second object") (weave [mvAccelDerivSents_2, mvAccelDerivEqns_2])

mvAccelDerivSents_2 :: [Sentence]
mvAccelDerivSents_2 = [S "The second object's acceleration is the geometric sum of the first object's acceleration and its own relative acceleration.",
                       S "This captures the coupling between the two pendulums through their mechanical connection.",
                       S "The Clifford algebra representation preserves the geometric relationships in this coupled system."]

mvAccelDerivEqns_2 :: [Sentence]
mvAccelDerivEqns_2 = [eS $ sy mvAccel_2 $= E.mvAccelExpr_2,
                      S "This represents the total acceleration as a vector sum in Clifford geometric space"]

-----------------------------------------------
-- Multivector Force for First Object       --
-----------------------------------------------
mvForceGD_1 :: GenDefn
mvForceGD_1 = gdNoRefs (equationalModel' mvForceQD_1) (getUnit force) (Just mvForceDeriv_1) "multivectorForce1" []

mvForceQD_1 :: ModelQDef
mvForceQD_1 = mkQuantDef' mvForce_1 (nounPhraseSP "multivector force on the first object") E.mvForceExpr_1

mvForceDeriv_1 :: Derivation
mvForceDeriv_1 = mkDerivName (phrase force +:+ S "on the first object") (weave [mvForceDerivSents_1, mvForceDerivEqns_1])

mvForceDerivSents_1 :: [Sentence]
mvForceDerivSents_1 = [S "The multivector force in Clifford algebra combines inertial and gravitational forces as vector multivectors.",
                       S "The inertial force is proportional to the multivector acceleration, preserving Newton's second law in geometric form.",
                       S "Gravitational force acts vertically downward and is naturally represented as a vector multivector."]

mvForceDerivEqns_1 :: [Sentence]
mvForceDerivEqns_1 = [eS $ sy mvForce_1 $= E.mvForceExpr_1,
                      S "where force equals mass times acceleration plus gravitational force in vector form"]

-----------------------------------------------
-- Multivector Force for Second Object      --
-----------------------------------------------
mvForceGD_2 :: GenDefn
mvForceGD_2 = gdNoRefs (equationalModel' mvForceQD_2) (getUnit force) (Just mvForceDeriv_2) "multivectorForce2" []

mvForceQD_2 :: ModelQDef
mvForceQD_2 = mkQuantDef' mvForce_2 (nounPhraseSP "multivector force on the second object") E.mvForceExpr_2

mvForceDeriv_2 :: Derivation
mvForceDeriv_2 = mkDerivName (phrase force +:+ S "on the second object") (weave [mvForceDerivSents_2, mvForceDerivEqns_2])

mvForceDerivSents_2 :: [Sentence]
mvForceDerivSents_2 = [S "The force on the second object combines its inertial response with gravitational effects.",
                       S "The Clifford algebra representation maintains the geometric consistency between force and acceleration.",
                       S "This approach naturally handles the coupling forces between the connected pendulum objects.",
                       S "Key GA operations demonstrated: geometric product (combines magnitude and direction),",
                       S "vector addition in Clifford space (preserves geometric relationships),",
                       S "scalar multiplication with multivectors (maintains geometric structure)."]

mvForceDerivEqns_2 :: [Sentence]
mvForceDerivEqns_2 = [eS $ sy mvForce_2 $= E.mvForceExpr_2,
                      S "This represents the total force as a vector sum in Clifford geometric space",
                      S "= mass × acceleration_multivector + gravitational_force_multivector",
                      S "Demonstrating how Newton's second law extends naturally to geometric algebra"]
