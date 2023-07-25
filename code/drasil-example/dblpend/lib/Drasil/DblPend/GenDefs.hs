{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPend.GenDefs (genDefns, velXGD_1, velYGD_1,
         accelXGD_1, accelYGD_1, accelXGD_2, accelYGD_2, xForceGD_1, yForceGD_1,
         xForceGD_2, yForceGD_2) where

import Prelude hiding (cos, sin, sqrt)
import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Utils.Drasil (weave)
import Theory.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import qualified Language.Drasil.NounPhrase.Combinators as NP
import Data.Drasil.Concepts.Math (xComp, yComp)
import Data.Drasil.Quantities.Physics (velocity, acceleration, force)
import Drasil.DblPend.DataDefs
import qualified Drasil.DblPend.Expressions as E
import qualified Drasil.DblPend.Derivations as D
import Drasil.DblPend.Unitals (lenRod_1, xVel_1, xVel_2,
    yVel_1, yVel_2, xAccel_1, yAccel_1, xAccel_2, yAccel_2)
import Drasil.DblPend.Concepts (horizontalPos,
    verticalPos, horizontalVel, verticalVel, horizontalForce, verticalForce, firstObject, secondObject)
import Control.Lens ((^.))

genDefns :: [GenDefn]
genDefns = [velXGD_1, velYGD_1, velXGD_2, velYGD_2, accelXGD_1, accelYGD_1, accelXGD_2, accelYGD_2,
       xForceGD_1, yForceGD_1, xForceGD_2, yForceGD_2]

------------------------------------------------
-- Velocity in X Direction in the First Object--
------------------------------------------------
velXGD_1 :: GenDefn
velXGD_1 = gdNoRefs (equationalModel' velXQD_1) (getUnit velocity) (Just velXDeriv_1) "velocityX1" [{-Notes-}]
-- general definiton block, with equation, unit, refinement explanation

velXQD_1 :: ModelQDef
velXQD_1 = mkQuantDef' xVel_1 (the xComp `NP.of_` (velocity `ofThe` firstObject)) E.velXExpr_1
-- lable and equation

velXDeriv_1 :: Derivation
velXDeriv_1 = mkDerivName (phraseNP (NP.the (xComp `of_` velocity))) (weave [velXDerivSents_1, velXDerivEqns_1])
-- title paragraph and weave the explained words and refined equation

velXDerivSents_1 :: [Sentence]
velXDerivSents_1 = [velDerivSent1,velXDerivSent2_1,velDerivSent3,velDerivSent4, velDerivSent5]
-- words used to explain the equation refinement

velXDerivEqns_1 :: [Sentence]
velXDerivEqns_1 = map eS [D.velDerivEqn1, D.velXDerivEqn2_1, D.velXDerivEqn3_1, D.velXDerivEqn4_1] ++ [eS' velXQD_1]
-- refinement equation after explained words

velDerivSent1, velXDerivSent2_1, velDerivSent3, velDerivSent4, velDerivSent5 :: Sentence
velDerivSent1 = S "At a given point in time" `sC` phrase velocity `S.is` definedIn'' positionGDD
velXDerivSent2_1 = S "We also know the" +:+ phrase horizontalPos +:+ S "that" `S.is` definedIn'' positionXDD_1
velDerivSent3 = S "Applying this,"
velDerivSent4 = eS' lenRod_1 `S.is` S "constant" `S.wrt` S  "time, so"
velDerivSent5 = S "Therefore, using the chain rule,"

------------------------------------------------
-- Velocity in Y Direction in the First Object--
------------------------------------------------
velYGD_1 :: GenDefn
velYGD_1 = gdNoRefs (equationalModel' velYQD_1) (getUnit velocity) (Just velYDeriv_1) "velocityY1" []

velYQD_1 :: ModelQDef
velYQD_1 = mkQuantDef' yVel_1 (the yComp `NP.of_` (velocity `ofThe` firstObject)) E.velYExpr_1
 
velYDeriv_1 :: Derivation
velYDeriv_1 = mkDerivName (phraseNP (NP.the (yComp `of_` velocity))) (weave [velYDerivSents_1, velYDerivEqns_1])

velYDerivSents_1 :: [Sentence]
velYDerivSents_1 = [velDerivSent1, velYDerivSent2_1, velDerivSent3, velDerivSent4, velDerivSent5]

velYDerivEqns_1 :: [Sentence]
velYDerivEqns_1 = map eS [D.velDerivEqn1, D.velYDerivEqn2_1, D.velYDerivEqn3_1, D.velYDerivEqn4_1] ++ [eS' velYQD_1]

velYDerivSent2_1 :: Sentence
velYDerivSent2_1 = S "We also know the" +:+ phrase verticalPos +:+ S "that" `S.is` definedIn'' positionYDD_1

-------------------------------------------------
-- Velocity in X Direction in the Second Object--
-------------------------------------------------
velXGD_2 :: GenDefn
velXGD_2 = gdNoRefs (equationalModel' velXQD_2) (getUnit velocity) (Just velXDeriv_2) "velocityX2" []

velXQD_2 :: ModelQDef
velXQD_2 = mkQuantDef' xVel_2 (the xComp `NP.of_` (velocity `ofThe` secondObject)) E.velXExpr_2

velXDeriv_2 :: Derivation
velXDeriv_2 = mkDerivName (phraseNP (NP.the (xComp `of_` velocity))) (weave [velXDerivSents_2, velXDerivEqns_2])

velXDerivSents_2 :: [Sentence]
velXDerivSents_2 = [velDerivSent1, velXDerivSent2_2, velDerivSent3, velDerivSent4]

velXDerivEqns_2 :: [Sentence]
velXDerivEqns_2 = map eS [D.velDerivEqn1, D.velXDerivEqn2_2, D.velXDerivEqn3_2] ++ [eS' velXQD_2] 

velXDerivSent2_2 :: Sentence
velXDerivSent2_2 = S "We also know the" +:+ phrase horizontalPos +:+ S "that" `S.is` definedIn'' positionXDD_2

-------------------------------------------------
-- Velocity in Y Direction in the Second Object--
-------------------------------------------------
velYGD_2 :: GenDefn
velYGD_2 = gdNoRefs (equationalModel' velYQD_2) (getUnit velocity) (Just velYDeriv_2) "velocityY2" []

velYQD_2 :: ModelQDef
velYQD_2 = mkQuantDef' yVel_2 (the yComp `NP.of_` (velocity `ofThe` secondObject)) E.velYExpr_2

velYDeriv_2 :: Derivation
velYDeriv_2 = mkDerivName (phraseNP (NP.the (yComp `of_` velocity))) (weave [velYDerivSents_2, velYDerivEqns_2])

velYDerivSents_2 :: [Sentence]
velYDerivSents_2 = [velDerivSent1,velYDerivSent2_2,velDerivSent3,velDerivSent5]

velYDerivEqns_2 :: [Sentence]
velYDerivEqns_2 = map eS [D.velDerivEqn1, D.velYDerivEqn2_2, D.velYDerivEqn3_2] ++ [eS' velYQD_2]

velYDerivSent2_2 :: Sentence
velYDerivSent2_2 = S "We also know the" +:+ phrase verticalPos +:+ S "that" `S.is` definedIn'' positionYDD_2

----------------------------------------------------
-- Acceleration in X direction in the First Object--
----------------------------------------------------
accelXGD_1 :: GenDefn
accelXGD_1 = gdNoRefs (equationalModel' accelXQD_1) (getUnit acceleration) (Just accelXDeriv_1) "accelerationX1" []

accelXQD_1 :: ModelQDef
accelXQD_1 = mkQuantDef' xAccel_1 (the xComp `NP.of_` (acceleration `ofThe` firstObject)) E.accelXExpr_1

accelXDeriv_1:: Derivation
accelXDeriv_1= mkDerivName (phraseNP (NP.the (xComp `of_` acceleration))) (weave [accelXDerivSents_1, accelXDerivEqns_1])

accelXDerivSents_1:: [Sentence]
accelXDerivSents_1= [accelDerivSent1, accelXDerivSent2_1, accelDerivSent3, accelDerivSent4, accelDerivSent5]

accelXDerivEqns_1 :: [Sentence]
accelXDerivEqns_1 = eS D.accelDerivEqn1 : eS' velXQD_1 : map eS [D.accelXDerivEqn3_1, D.accelXDerivEqn4_1] ++ [eS' accelXQD_1]

accelDerivSent1, accelXDerivSent2_1, accelDerivSent3, accelDerivSent4, accelDerivSent5 :: Sentence

accelDerivSent1 = S "Our" +:+ phrase acceleration +: S "is"
accelXDerivSent2_1 = S "Earlier" `sC` S "we found the" +:+ phrase horizontalVel +:+ S "to be"
accelDerivSent3 = S "Applying this to our equation for" +:+ phrase acceleration
accelDerivSent4 = S "By the product and chain rules, we find"
accelDerivSent5 = S "Simplifying,"

----------------------------------------------------
-- Acceleration in Y direction in the First Object--
----------------------------------------------------
accelYGD_1 :: GenDefn
accelYGD_1 = gdNoRefs (equationalModel' accelYQD_1) (getUnit acceleration) (Just accelYDeriv_1) "accelerationY1" []

accelYQD_1 :: ModelQDef
accelYQD_1 = mkQuantDef' yAccel_1 (the yComp `NP.of_` (acceleration `ofThe` firstObject)) E.accelYExpr_1

accelYDeriv_1:: Derivation
accelYDeriv_1= mkDerivName (phraseNP (NP.the (yComp `of_` acceleration))) (weave [accelYDerivSents_1, accelYDerivEqns_1])

accelYDerivSents_1 :: [Sentence]
accelYDerivSents_1 = [accelDerivSent1, accelYDerivSent2_1, accelDerivSent3, accelDerivSent4, accelDerivSent5]

accelYDerivEqns_1 :: [Sentence]
accelYDerivEqns_1 = eS D.accelDerivEqn1 : eS' velYQD_1 : map eS [D.accelYDerivEqn3_1, D.accelYDerivEqn4_1] ++ [eS' accelYQD_1]

accelYDerivSent2_1 :: Sentence
accelYDerivSent2_1 = S "Earlier" `sC` S "we found the" +:+ phrase verticalVel +:+ S "to be"

-----------------------------------------------------
-- Acceleration in X direction in the Second Object--
-----------------------------------------------------
accelXGD_2 :: GenDefn
accelXGD_2 = gdNoRefs (equationalModel' accelXQD_2) (getUnit acceleration) (Just accelXDeriv_2) "accelerationX2" []

accelXQD_2 :: ModelQDef
accelXQD_2 = mkQuantDef' xAccel_2 (the xComp `NP.of_` (acceleration `ofThe` secondObject)) E.accelXExpr_2

accelXDeriv_2:: Derivation
accelXDeriv_2= mkDerivName (phraseNP (NP.the (xComp `of_` acceleration))) (weave [accelXDerivSents_2, accelXDerivEqns_2])

accelXDerivSents_2:: [Sentence]
accelXDerivSents_2= [accelDerivSent1, accelXDerivSent2_2, accelDerivSent3, accelDerivSent4]

accelXDerivEqns_2 :: [Sentence]
accelXDerivEqns_2 = eS D.accelDerivEqn1 : eS' velXQD_2 : [eS D.accelXDerivEqn3_2, eS' accelXQD_2]

accelXDerivSent2_2 :: Sentence
accelXDerivSent2_2 = S "Earlier" `sC` S "we found the" +:+ phrase horizontalVel +:+ S "to be"

-----------------------------------------------------
-- Acceleration in Y direction in the Second Object--
-----------------------------------------------------
accelYGD_2 :: GenDefn
accelYGD_2 = gdNoRefs (equationalModel' accelYQD_2) (getUnit acceleration) (Just accelYDeriv_2) "accelerationY2" []

accelYQD_2 :: ModelQDef
accelYQD_2 = mkQuantDef' yAccel_2 (the yComp `NP.of_` (acceleration `ofThe` secondObject)) E.accelYExpr_2

accelYDeriv_2:: Derivation
accelYDeriv_2= mkDerivName (phraseNP (NP.the (yComp `of_` acceleration))) (weave [accelYDerivSents_2, accelYDerivEqns_2])

accelYDerivSents_2:: [Sentence]
accelYDerivSents_2= [accelDerivSent1, accelYDerivSent2_2, accelDerivSent3, accelDerivSent4]

accelYDerivEqns_2 :: [Sentence]
accelYDerivEqns_2 = eS D.accelDerivEqn1 : eS' velYQD_2 : [eS D.accelYDerivEqn3_2, eS' accelYQD_2]

accelYDerivSent2_2 :: Sentence
accelYDerivSent2_2 = S "Earlier" `sC` S "we found the" +:+ phrase horizontalVel +:+ S "to be"

-------------------------------------------------
-- Horizontal force acting on the first object --
-------------------------------------------------
xForceGD_1 :: GenDefn
xForceGD_1 = gdNoRefs (equationalRealmU "xForce1" xForceMD_1)
        (getUnit force) (Just xForceDeriv_1) "xForce1" []

xForceMD_1 :: MultiDefn ModelExpr
xForceMD_1 = mkMultiDefnForQuant quant EmptyS defns
    where quant = mkQuant' "force" (horizontalForce `onThe` firstObject)
                    Nothing Real (symbol force) (getUnit force)
          defns = NE.fromList [
                    mkDefiningExpr "xForceWithMass1"
                      [] EmptyS $ express $ forceGQD ^. defnExpr,
                    mkDefiningExpr "xForceWithAngle1"
                      [] EmptyS E.xForceWithAngle_1]

xForceDeriv_1 :: Derivation
xForceDeriv_1 = mkDerivName (phraseNP (force `onThe` firstObject)) [eS' xForceMD_1]

-------------------------------------------------
-- Vertical force acting on the first object --
-------------------------------------------------
yForceGD_1 :: GenDefn
yForceGD_1 = gdNoRefs (equationalRealmU "yForce1" yForceMD_1)
        (getUnit force) (Just yForceDeriv_1) "yForce1" []

yForceMD_1 :: MultiDefn ModelExpr
yForceMD_1 = mkMultiDefnForQuant quant EmptyS defns
    where quant = mkQuant' "force" (verticalForce `onThe` firstObject)
                    Nothing Real (symbol force) (getUnit force)
          defns = NE.fromList [
                    mkDefiningExpr "yForceWithMass1"
                      [] EmptyS $ express $ forceGQD ^. defnExpr,
                    mkDefiningExpr "yForceWithAngle1"
                      [] EmptyS E.yForceWithAngle_1]

yForceDeriv_1 :: Derivation
yForceDeriv_1 = mkDerivName (phraseNP (force `onThe` firstObject)) [eS' yForceMD_1]

-------------------------------------------------
-- Horizontal force acting on the second object --
-------------------------------------------------
xForceGD_2 :: GenDefn
xForceGD_2 = gdNoRefs (equationalRealmU "xForce2" xForceMD_2)
        (getUnit force) (Just xForceDeriv_2) "xForce2" []

xForceMD_2 :: MultiDefn ModelExpr
xForceMD_2 = mkMultiDefnForQuant quant EmptyS defns
    where quant = mkQuant' "force" (horizontalForce `onThe` secondObject)
                    Nothing Real (symbol force) (getUnit force)
          defns = NE.fromList [
                    mkDefiningExpr "xForceWithMass2"
                      [] EmptyS $ express $ forceGQD ^. defnExpr,
                    mkDefiningExpr "xForceWithAngle2"
                      [] EmptyS E.xForceWithAngle_2]

xForceDeriv_2 :: Derivation
xForceDeriv_2 = mkDerivName (phraseNP (force `onThe` secondObject)) [eS' xForceMD_2]

-------------------------------------------------
-- Vertical force acting on the first object --
-------------------------------------------------
yForceGD_2 :: GenDefn
yForceGD_2 = gdNoRefs (equationalRealmU "yForce2" yForceMD_2)
        (getUnit force) (Just yForceDeriv_2) "yForce2" []

yForceMD_2 :: MultiDefn ModelExpr
yForceMD_2 = mkMultiDefnForQuant quant EmptyS defns
    where quant = mkQuant' "force" (verticalForce `onThe` secondObject)
                    Nothing Real (symbol force) (getUnit force)
          defns = NE.fromList [
                    mkDefiningExpr "yForceWithMass2"
                      [] EmptyS $ express $ forceGQD ^. defnExpr,
                    mkDefiningExpr "yForceWithAngle2"
                      [] EmptyS E.yForceWithAngle_2]

yForceDeriv_2 :: Derivation
yForceDeriv_2 = mkDerivName (phraseNP (force `onThe` secondObject)) [eS' yForceMD_2]
