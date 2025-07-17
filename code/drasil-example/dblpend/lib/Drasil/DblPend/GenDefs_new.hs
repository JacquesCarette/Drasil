{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPend.GenDefs_new (genDefns, mvVelGD_1, mvVelGD_2, 
         mvAccelGD_1, mvAccelGD_2, mvForceGD_1, mvForceGD_2) where

import Prelude hiding (cos, sin, sqrt)

import Language.Drasil (ModelQDef, Derivation, Sentence(..), ModelExpr, mkDerivName, phraseNP, eS,
  (+:+), phrase, getUnit, mkQuantDef', express, defnExpr)
import Language.Drasil.Space (Space(Real))
import Utils.Drasil (weave)
import Theory.Drasil (GenDefn, equationalModel', gdNoRefs)
import Language.Drasil.Chunk.Concept.NamedCombinators
import Data.Drasil.Quantities.Physics (velocity, acceleration, force)
import qualified Drasil.DblPend.Expressions as E
import Drasil.DblPend.Concepts (firstObject, secondObject)

genDefns :: [GenDefn]
genDefns = [mvVelGD_1, mvVelGD_2, mvAccelGD_1, mvAccelGD_2, mvForceGD_1, mvForceGD_2]

-----------------------------------------------
-- Multivector Velocity for First Object    --
-----------------------------------------------
mvVelGD_1 :: GenDefn
mvVelGD_1 = gdNoRefs (equationalModel' mvVelQD_1) (getUnit velocity) (Just mvVelDeriv_1) "multivectorVelocity1" []

mvVelQD_1 :: ModelQDef  
mvVelQD_1 = express E.mvVelExpr_1 (phraseNP (velocity `ofThe` firstObject)) "mv_vel_1"

mvVelDeriv_1 :: Derivation
mvVelDeriv_1 = mkDerivName (phraseNP (velocity `ofThe` firstObject)) (weave [mvVelDerivSents_1, mvVelDerivEqns_1])

mvVelDerivSents_1 :: [Sentence]
mvVelDerivSents_1 = [S "The multivector velocity represents the geometric relationship in Clifford algebra space."]

mvVelDerivEqns_1 :: [ModelExpr]  
mvVelDerivEqns_1 = [E.mvVelExpr_1]

-----------------------------------------------
-- Multivector Velocity for Second Object   --
-----------------------------------------------
mvVelGD_2 :: GenDefn
mvVelGD_2 = gdNoRefs (equationalModel' mvVelQD_2) (getUnit velocity) (Just mvVelDeriv_2) "multivectorVelocity2" []

mvVelQD_2 :: ModelQDef
mvVelQD_2 = express E.mvVelExpr_2 (phraseNP (velocity `ofThe` secondObject)) "mv_vel_2"

mvVelDeriv_2 :: Derivation
mvVelDeriv_2 = mkDerivName (phraseNP (velocity `ofThe` secondObject)) (weave [mvVelDerivSents_2, mvVelDerivEqns_2])

mvVelDerivSents_2 :: [Sentence]
mvVelDerivSents_2 = [S "The multivector velocity for the second object follows similar Clifford algebra principles."]

mvVelDerivEqns_2 :: [ModelExpr]
mvVelDerivEqns_2 = [E.mvVelExpr_2]

-----------------------------------------------
-- Multivector Acceleration for First Object--
-----------------------------------------------
mvAccelGD_1 :: GenDefn
mvAccelGD_1 = gdNoRefs (equationalModel' mvAccelQD_1) (getUnit acceleration) (Just mvAccelDeriv_1) "multivectorAcceleration1" []

mvAccelQD_1 :: ModelQDef
mvAccelQD_1 = express E.mvAccelExpr_1 (phraseNP (acceleration `ofThe` firstObject)) "mv_accel_1"

mvAccelDeriv_1 :: Derivation
mvAccelDeriv_1 = mkDerivName (phraseNP (acceleration `ofThe` firstObject)) (weave [mvAccelDerivSents_1, mvAccelDerivEqns_1])

mvAccelDerivSents_1 :: [Sentence]
mvAccelDerivSents_1 = [S "The multivector acceleration captures rotational and translational dynamics in Clifford space."]

mvAccelDerivEqns_1 :: [ModelExpr]
mvAccelDerivEqns_1 = [E.mvAccelExpr_1]

-----------------------------------------------
-- Multivector Acceleration for Second Object--
-----------------------------------------------
mvAccelGD_2 :: GenDefn  
mvAccelGD_2 = gdNoRefs (equationalModel' mvAccelQD_2) (getUnit acceleration) (Just mvAccelDeriv_2) "multivectorAcceleration2" []

mvAccelQD_2 :: ModelQDef
mvAccelQD_2 = express E.mvAccelExpr_2 (phraseNP (acceleration `ofThe` secondObject)) "mv_accel_2"

mvAccelDeriv_2 :: Derivation
mvAccelDeriv_2 = mkDerivName (phraseNP (acceleration `ofThe` secondObject)) (weave [mvAccelDerivSents_2, mvAccelDerivEqns_2])

mvAccelDerivSents_2 :: [Sentence]
mvAccelDerivSents_2 = [S "The second object's multivector acceleration follows from the geometric product in Clifford algebra."]

mvAccelDerivEqns_2 :: [ModelExpr]
mvAccelDerivEqns_2 = [E.mvAccelExpr_2]

-----------------------------------------------
-- Multivector Force for First Object       --
-----------------------------------------------
mvForceGD_1 :: GenDefn
mvForceGD_1 = gdNoRefs (equationalModel' mvForceQD_1) (getUnit force) (Just mvForceDeriv_1) "multivectorForce1" []

mvForceQD_1 :: ModelQDef
mvForceQD_1 = express E.mvForceExpr_1 (phraseNP (force `ofThe` firstObject)) "mv_force_1"

mvForceDeriv_1 :: Derivation
mvForceDeriv_1 = mkDerivName (phraseNP (force `ofThe` firstObject)) (weave [mvForceDerivSents_1, mvForceDerivEqns_1])

mvForceDerivSents_1 :: [Sentence]
mvForceDerivSents_1 = [S "The multivector force incorporates both magnitude and direction through Clifford algebra."]

mvForceDerivEqns_1 :: [ModelExpr]
mvForceDerivEqns_1 = [E.mvForceExpr_1]

-----------------------------------------------
-- Multivector Force for Second Object      --
-----------------------------------------------
mvForceGD_2 :: GenDefn
mvForceGD_2 = gdNoRefs (equationalModel' mvForceQD_2) (getUnit force) (Just mvForceDeriv_2) "multivectorForce2" []

mvForceQD_2 :: ModelQDef
mvForceQD_2 = express E.mvForceExpr_2 (phraseNP (force `ofThe` secondObject)) "mv_force_2"

mvForceDeriv_2 :: Derivation
mvForceDeriv_2 = mkDerivName (phraseNP (force `ofThe` secondObject)) (weave [mvForceDerivSents_2, mvForceDerivEqns_2])

mvForceDerivSents_2 :: [Sentence]
mvForceDerivSents_2 = [S "The second object's multivector force follows the same Clifford algebra principles."]

mvForceDerivEqns_2 :: [ModelExpr]
mvForceDerivEqns_2 = [E.mvForceExpr_2]
