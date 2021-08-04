{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPendulum.GenDefs (genDefns, velIXGD_1, velIYGD_1,
         accelerationIXGD, accelerationIYGD, hForceOnPendulumGD, vForceOnPendulumGD,
         angFrequencyGD, periodPend) where

import Prelude hiding (cos, sin, sqrt)
import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs,
    equationalModel', equationalModelU, equationalRealmU,
    MultiDefn, mkDefiningExpr, mkMultiDefnForQuant)
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S
import qualified Utils.Drasil.NounPhrase as NP

-- import Data.Drasil.Concepts.Documentation (coordinate, symbol_)
import Data.Drasil.Concepts.Math (xComp, yComp, equation, component, direction, angle)
import Data.Drasil.Quantities.Physics (velocity, acceleration, force,
    momentOfInertia, torque, angularDisplacement, angularFrequency,
    frequency, period, xAccel, yAccel)
import Data.Drasil.Concepts.Physics (pendulum, weight, shm)
import Data.Drasil.Quantities.PhysicalProperties (mass, len)
import Data.Drasil.Theories.Physics (newtonSLR)
import Drasil.DblPendulum.DataDefs (positionGDD, frequencyDD, periodSHMDD, angFrequencyDD)

-- import Drasil.Projectile.Assumptions (cartSyst, constAccel, pointMass, timeStartZero, twoDMotion)
import qualified Drasil.DblPendulum.Expressions as E
import Drasil.DblPendulum.Unitals (lenRod, pendDisplacementAngle, lenRod_1, lenRod_2, xVel_1, xVel_2, yVel_1, yVel_2)
import Drasil.DblPendulum.Concepts (arcLen, horizontalPos,
    verticalPos, horizontalVel, verticalVel, horizontalForce, verticalForce, firstObject, secondObject)

genDefns :: [GenDefn]
genDefns = [velIXGD_1, velIYGD_1, velIXGD_2, accelerationIXGD, accelerationIYGD,
       hForceOnPendulumGD, vForceOnPendulumGD, angFrequencyGD, periodPend]

-----------------------------------------------
-- Velocity in X Dirction in the First Object--
-----------------------------------------------
velIXGD_1 :: GenDefn
velIXGD_1 = gdNoRefs (equationalModel' velIXQD_1) (getUnit velocity)
           (Just velIXDeriv_1) "velocityIX1" [{-Notes-}]
-- general definiton block, with equation, unit, refinement explanation

velIXQD_1 :: QDefinition
velIXQD_1 = mkQuantDef' xVel_1 (the xComp `NP.of_` (velocity `ofThe` firstObject)) E.velIXExpr_1 -- lable and equation

velIXDeriv_1 :: Derivation
velIXDeriv_1 = mkDerivName (phraseNP (NP.the (xComp `of_` velocity))) (weave [velIXDerivSents_1, velIXDerivEqns_1])
-- title paragraph and weave the explained words and refined equation

velIXDerivSents_1 :: [Sentence]
velIXDerivSents_1 = [velDerivSent1,velIXDerivSent2_1,velIXDerivSent3_1,velIXDerivSent4_1, velIXDerivSent5_1]
-- words used to explain the equation refinement

velIXDerivEqns_1 :: [Sentence]
velIXDerivEqns_1 = map eS [E.velIXDerivEqn1_1, E.velIXDerivEqn2_1,
    E.velIXDerivEqn3_1, E.velIXDerivEqn4_1] ++ [eS velIXQD_1] -- refinement equation after explained words

velDerivSent1, velIXDerivSent2_1, velIXDerivSent3_1, velIXDerivSent4_1, velIXDerivSent5_1 :: Sentence
velDerivSent1 = S "At a given point in time" `sC` phrase velocity `S.is` definedIn'' positionGDD
velIXDerivSent2_1 = S "We also know the" +:+ phrase horizontalPos
velIXDerivSent3_1 = S "Applying this,"
velIXDerivSent4_1 = eS lenRod_1 `S.is` S "constant" `S.wrt` S  "time, so"
velIXDerivSent5_1 = S "Therefore, using the chain rule,"

-----------------------------------------------
-- Velocity in Y Dirction in the First Object--
-----------------------------------------------
velIYGD_1 :: GenDefn
velIYGD_1 = gdNoRefs (equationalModel' velIYQD_1) (getUnit velocity)
           (Just velIYDeriv_1) "velocityIY1" [{-Notes-}]

velIYQD_1 :: QDefinition
velIYQD_1 = mkQuantDef' yVel_1 (the yComp `NP.of_` (velocity `ofThe` firstObject)) E.velIYExpr_1
 
velIYDeriv_1 :: Derivation
velIYDeriv_1 = mkDerivName (phraseNP (NP.the (yComp `of_` velocity))) (weave [velIYDerivSents_1, velIYDerivEqns_1])

velIYDerivSents_1 :: [Sentence]
velIYDerivSents_1 = [velDerivSent1, velIYDerivSent2_1, velIYDerivSent3_1, velIYDerivSent4_1, velIYDerivSent5_1]

velIYDerivEqns_1 :: [Sentence]
velIYDerivEqns_1 = map eS [E.velIYDerivEqn1_1, E.velIYDerivEqn2_1,
    E.velIYDerivEqn3_1, E.velIYDerivEqn4_1] ++ [eS velIYQD_1]

velIYDerivSent2_1, velIYDerivSent3_1, velIYDerivSent4_1, velIYDerivSent5_1 :: Sentence
velIYDerivSent2_1 = S "We also know the" +:+ phrase verticalPos
velIYDerivSent3_1 = S "Applying this again,"
velIYDerivSent4_1 = eS lenRod_1 `S.is` S "constant" `S.wrt` S "time, so"
velIYDerivSent5_1 = S "Therefore, using the chain rule,"

------------------------------------------------
-- Velocity in X Dirction in the Second Object--
------------------------------------------------
velIXGD_2 :: GenDefn
velIXGD_2 = gdNoRefs (equationalModel' velIXQD_2) (getUnit velocity)
           (Just velIXDeriv_2) "velocityIX2" [{-Notes-}]
-- general definiton block, with equation, unit, refinement explanation

velIXQD_2 :: QDefinition
velIXQD_2 = mkQuantDef' xVel_2 (the xComp `NP.of_` (velocity `ofThe` secondObject)) E.velIXExpr_2 -- lable and equation

velIXDeriv_2 :: Derivation
velIXDeriv_2 = mkDerivName (phraseNP (NP.the (xComp `of_` velocity))) (weave [velIXDerivSents_2, velIXDerivEqns_2])
-- title paragraph and weave the explained words and refined equation

velIXDerivSents_2 :: [Sentence]
velIXDerivSents_2 = [velDerivSent1,velIXDerivSent2_2,velIXDerivSent3_2,velIXDerivSent4_2]
-- words used to explain the equation refinement

velIXDerivEqns_2 :: [Sentence]
velIXDerivEqns_2 = map eS [E.velIXDerivEqn1_2, E.velIXDerivEqn2_2,
    E.velIXDerivEqn3_2] ++ [eS velIXQD_2] -- refinement equation after explained words

velIXDerivSent2_2, velIXDerivSent3_2, velIXDerivSent4_2 :: Sentence
velIXDerivSent2_2 = S "We also know the" +:+ phrase horizontalPos
velIXDerivSent3_2 = S "Applying this,"
velIXDerivSent4_2 = S "Therefore, using the chain rule,"

-----------------------
accelerationIXGD :: GenDefn
accelerationIXGD = gdNoRefs (equationalModel' accelerationIXQD) (getUnit acceleration)
           (Just accelerationIXDeriv) "accelerationIX" [{-Notes-}]

accelerationIXQD :: QDefinition
accelerationIXQD = mkQuantDef' xAccel (the xComp `NP.of_` (acceleration `ofThe` pendulum))
    E.accelerationIXExpr

accelerationIXDeriv :: Derivation
accelerationIXDeriv = mkDerivName (phraseNP (NP.the (xComp `of_` acceleration))) (weave [accelerationIXDerivSents, accelerationIXDerivEqns])

accelerationIXDerivSents :: [Sentence]
accelerationIXDerivSents = [accelerationIDerivSent1, accelerationIXDerivSent2, accelerationIXDerivSent3,
    accelerationIXDerivSent4, accelerationIXDerivSent5]

accelerationIXDerivEqns :: [Sentence]
accelerationIXDerivEqns = eS E.accelerationIDerivEqn1 : eS velIXQD_1 :
    map eS [E.accelerationIXDerivEqn3, E.accelerationIXDerivEqn4] ++ [eS accelerationIXQD]

accelerationIDerivSent1, accelerationIXDerivSent2, accelerationIXDerivSent3,
     accelerationIXDerivSent4, accelerationIXDerivSent5 :: Sentence

accelerationIDerivSent1 = S "Our" +:+ phrase acceleration +: S "is"
accelerationIXDerivSent2 = S "Earlier" `sC` S "we found the" +:+ phrase horizontalVel +:+ S "to be"
accelerationIXDerivSent3 = S "Applying this to our equation for" +:+ phrase acceleration
accelerationIXDerivSent4 = S "By the product and chain rules, we find"
accelerationIXDerivSent5 = S "Simplifying,"

-----------------------
accelerationIYGD :: GenDefn
accelerationIYGD = gdNoRefs (equationalModel' accelerationIYQD) (getUnit acceleration)
           (Just accelerationIYDeriv) "accelerationIY" [{-Notes-}]

accelerationIYQD :: QDefinition
accelerationIYQD = mkQuantDef' yAccel (the yComp `NP.of_` (acceleration `ofThe` pendulum)) E.accelerationIYExpr

accelerationIYDeriv :: Derivation
accelerationIYDeriv = mkDerivName (phraseNP (NP.the (yComp `of_` acceleration))) (weave [accelerationIYDerivSents, accelerationIYDerivEqns])

accelerationIYDerivSents :: [Sentence]
accelerationIYDerivSents = [accelerationIDerivSent1, accelerationIYDerivSent2, accelerationIYDerivSent3,
    accelerationIYDerivSent4, accelerationIYDerivSent5]

accelerationIYDerivEqns :: [Sentence]
accelerationIYDerivEqns = eS E.accelerationIDerivEqn1 : eS velIYQD_1 :
    map eS [E.accelerationIYDerivEqn3, E.accelerationIYDerivEqn4] ++ [eS accelerationIYQD]

accelerationIYDerivSent2, accelerationIYDerivSent3, accelerationIYDerivSent4,
    accelerationIYDerivSent5 :: Sentence
accelerationIYDerivSent2 = S "Earlier" `sC` S "we found the" +:+ phrase verticalVel +:+ S "to be"
accelerationIYDerivSent3 = S "Applying this to our equation for" +:+ phrase acceleration
accelerationIYDerivSent4 = S "By the product and chain rules, we find"
accelerationIYDerivSent5 = S "Simplifying,"

-------------------------------------Horizontal force acting on the pendulum 
hForceOnPendulumGD :: GenDefn
hForceOnPendulumGD = gdNoRefs (equationalRealmU "hForceOnPendulum" hForceOnPendulumMD)
        (getUnit force) (Just hForceOnPendulumDeriv) "hForceOnPendulum" [{-Notes-}]

hForceOnPendulumMD :: MultiDefn
hForceOnPendulumMD = mkMultiDefnForQuant quant EmptyS defns
    where quant = mkQuant' "force" (horizontalForce `onThe` pendulum)
                    Nothing Real (symbol force) (getUnit force)
          defns = NE.fromList [
                    mkDefiningExpr "hForceOnPendulumViaComponent"
                      [] EmptyS E.hForceOnPendulumViaComponent,
                    mkDefiningExpr "hForceOnPendulumViaAngle"
                      [] EmptyS E.hForceOnPendulumViaAngle
                  ]

hForceOnPendulumDeriv :: Derivation
hForceOnPendulumDeriv = mkDerivName (phraseNP (force `onThe` pendulum)) [eS hForceOnPendulumMD]

----------------------------------------Vertical force acting on the pendulum 
vForceOnPendulumGD :: GenDefn
vForceOnPendulumGD = gdNoRefs (equationalRealmU "vForceOnPendulum" vForceOnPendulumMD)
        (getUnit force) (Just vForceOnPendulumDeriv) "vForceOnPendulum" [{-Notes-}]

vForceOnPendulumMD :: MultiDefn
vForceOnPendulumMD = mkMultiDefnForQuant quant EmptyS defns
    where quant = mkQuant' "force" (verticalForce `onThe` pendulum)
                    Nothing Real (symbol force) (getUnit force)
          defns = NE.fromList [
                    mkDefiningExpr "vForceOnPendulumViaComponent"
                      [] EmptyS E.vForceOnPendulumViaComponent ,
                    mkDefiningExpr "vForceOnPendulumViaAngle"    
                      [] EmptyS E.vForceOnPendulumViaAngle
                  ]

vForceOnPendulumDeriv :: Derivation
vForceOnPendulumDeriv = mkDerivName (phraseNP (force `onThe` pendulum)) [eS vForceOnPendulumMD]

--------------------------------------Angular Frequency Of Pendulum

angFrequencyGD :: GenDefn
angFrequencyGD = gdNoRefs (equationalModelU "angFrequencyGD" angFrequencyQD) (getUnit angularFrequency)
           (Just angFrequencyDeriv) "angFrequencyGD" [angFrequencyGDNotes]

angFrequencyQD :: QDefinition
angFrequencyQD = mkQuantDef' angularFrequency (angularFrequency `the_ofThe` pendulum) E.angFrequencyExpr

angFrequencyDeriv :: Derivation
angFrequencyDeriv = mkDerivName (phraseNP (angularFrequency `the_ofThe` pendulum)) (weave [angFrequencyDerivSents, map eS E.angFrequencyDerivEqns])


angFrequencyDerivSents :: [Sentence]
angFrequencyDerivSents = [angFrequencyDerivSent1, angFrequencyDerivSent2, angFrequencyDerivSent3,
                      angFrequencyDerivSent4, angFrequencyDerivSent5, angFrequencyDerivSent6, angFrequencyDerivSent7]

angFrequencyDerivSent1, angFrequencyDerivSent2, angFrequencyDerivSent3,
     angFrequencyDerivSent4, angFrequencyDerivSent5, angFrequencyDerivSent6, angFrequencyDerivSent7 :: Sentence
angFrequencyDerivSent1 = foldlSentCol [S "Consider the", phrase torque, S "on a", phrase pendulum +:+. definedIn'' newtonSLR,
                  S "The", phrase force, S "providing the restoring", phrase torque `S.is` phraseNP (the component `NP.of_`
                  (weight `ofThe` pendulum)), S "bob that acts along the" +:+. phrase arcLen,
                  (phrase torque `S.isThe` phrase len) `S.the_ofTheC` S "string", ch lenRod, S "multiplied by", phrase component
                  `S.the_ofThe` S "net", phrase force, S "that is perpendicular to", S "radius" `S.the_ofThe` (S "arc" !.),
                  S "The minus sign indicates the", phrase torque, S "acts in the opposite", phraseNP (direction `ofThe`angularDisplacement)]
angFrequencyDerivSent2 = S "So then"
angFrequencyDerivSent3 = S "Therefore,"
angFrequencyDerivSent4 = S "Substituting for" +:+ ch momentOfInertia
angFrequencyDerivSent5 = S "Crossing out" +:+ ch mass `S.and_` ch lenRod +:+ S "we have"
angFrequencyDerivSent6 = S "For small" +:+ plural angle `sC` S "we approximate" +:+ S "sin" +:+ ch pendDisplacementAngle +:+ S "to" +:+ ch pendDisplacementAngle
angFrequencyDerivSent7 = S "Because this" +:+ phrase equation `sC` S "has the same form as the" +:+ phraseNP (equation `for` shm) +:+. 
                        S "the solution is easy to find" +:+ S " The" +:+ phrase angularFrequency

angFrequencyGDNotes :: Sentence
angFrequencyGDNotes = S "The" +:+ phrase torque `S.is` definedIn'' newtonSLR  `S.and_` phrase frequency `S.is` definedIn frequencyDD

 -------------------------------- Period of Pendulum Motion 

periodPend :: GenDefn
periodPend = gdNoRefs (equationalModelU "periodPendGD" periodPendQD) (getUnit period)
           (Just periodPendDeriv) "periodPend" [periodPendNotes]

periodPendQD :: QDefinition
periodPendQD = mkQuantDef' period (NP.the (period `ofThe` pendulum)) E.periodPendExpr

periodPendDeriv :: Derivation
periodPendDeriv = mkDerivName (phraseNP (NP.the (period `ofThe` pendulum))) (weave [periodPendDerivSents, map eS E.periodPendDerivEqns])

periodPendDerivSents :: [Sentence]
periodPendDerivSents = [periodPendDerivSent1, periodPendDerivSent2]

periodPendDerivSent1, periodPendDerivSent2 :: Sentence
periodPendDerivSent1 = atStartNP (period `the_ofThe` pendulum) +:+ S "can be defined from the general definition for the" +:+ phrase equation `S.of_`
                namedRef angFrequencyGD (phrase angFrequencyDD)
periodPendDerivSent2 =  S "Therefore from the data definition of the" +:+ phrase equation `S.for` namedRef angFrequencyDD (phrase angFrequencyDD) `sC` S "we have"

periodPendNotes :: Sentence
periodPendNotes = atStartNP (NP.the (frequency `and_` period)) +:+ S "are defined in the data definitions for" +:+ namedRef frequencyDD (phrase frequencyDD) `S.and_`
        namedRef periodSHMDD (phrase periodSHMDD) +:+ S "respectively"
