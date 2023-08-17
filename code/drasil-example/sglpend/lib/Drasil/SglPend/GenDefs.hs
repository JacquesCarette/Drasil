{-# LANGUAGE PostfixOperators #-}
module Drasil.SglPend.GenDefs (genDefns, velocityIXGD, velocityIYGD,
         accelerationIXGD, accelerationIYGD, hForceOnPendulumGD, vForceOnPendulumGD,
         angFrequencyGD, periodPend) where

import Prelude hiding (cos, sin, sqrt)
import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs,
    equationalModel', equationalModelU, equationalRealmU,
    MultiDefn, mkDefiningExpr, mkMultiDefnForQuant)
import Utils.Drasil (weave)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import qualified Language.Drasil.NounPhrase.Combinators as NP

-- import Data.Drasil.Concepts.Documentation (coordinate, symbol_)
import Data.Drasil.Concepts.Math (xComp, yComp, equation, component, direction, angle)
import Data.Drasil.Quantities.Physics (velocity, acceleration, force,
    momentOfInertia, torque, angularDisplacement, angularFrequency,
    frequency, period, xAccel, xVel, yAccel, yVel)
import Data.Drasil.Concepts.Physics (pendulum, weight, shm)
import Data.Drasil.Quantities.PhysicalProperties (mass, len)
import Data.Drasil.Theories.Physics (newtonSLR)
import Drasil.SglPend.DataDefs (frequencyDD, periodSHMDD, angFrequencyDD)

-- import Drasil.Projectile.Assumptions (cartSyst, constAccel, pointMass, timeStartZero, twoDMotion)
import qualified Drasil.SglPend.Derivations as D
import qualified Drasil.SglPend.Expressions as E
import Drasil.SglPend.Unitals (lenRod, pendDisplacementAngle)
import Drasil.DblPend.Concepts (arcLen, horizontalPos,
    verticalPos, horizontalVel, verticalVel, horizontalForce, verticalForce)

genDefns :: [GenDefn]
genDefns = [velocityIXGD, velocityIYGD, accelerationIXGD, accelerationIYGD,
       hForceOnPendulumGD, vForceOnPendulumGD, angFrequencyGD, periodPend]

---------------------
velocityIXGD :: GenDefn
velocityIXGD = gdNoRefs (equationalModel' velocityIXQD) (getUnit velocity)
           (Just velocityIXDeriv) "velocityIX" [{-Notes-}]

velocityIXQD :: ModelQDef
velocityIXQD = mkQuantDef' xVel (the xComp `NP.of_` (velocity `ofThe` pendulum))
    $ express E.velocityIXExpr

velocityIXDeriv :: Derivation
velocityIXDeriv = mkDerivName (phraseNP (NP.the (xComp `of_` velocity))) (weave [velocityIXDerivSents, velocityIXDerivEqns])

velocityIXDerivSents :: [Sentence]
velocityIXDerivSents = [velocityDerivSent1, velocityIXDerivSent2, velocityDerivSent3,
                        velocityDerivSent4, velocityDerivSent5]

velocityIXDerivEqns :: [Sentence]
velocityIXDerivEqns = map eS D.velocityIXDerivEqns ++ [eS' velocityIXQD]

velocityDerivSent1, velocityIXDerivSent2, velocityDerivSent3,
    velocityDerivSent4, velocityDerivSent5 :: Sentence
velocityDerivSent1 = S "At a given point in time" `sC` phrase velocity +:+ S "may be defined as"
velocityIXDerivSent2 = S "We also know the" +:+ phrase horizontalPos
velocityDerivSent3 = S "Applying this,"
velocityDerivSent4 = eS' lenRod `S.is` S "constant" `S.wrt` S  "time, so"
velocityDerivSent5 = S "Therefore, using the chain rule,"

---------------------
velocityIYGD :: GenDefn
velocityIYGD = gdNoRefs (equationalModel' velocityIYQD) (getUnit velocity)
           (Just velocityIYDeriv) "velocityIY" [{-Notes-}]

velocityIYQD :: ModelQDef
velocityIYQD = mkQuantDef' yVel (the yComp `NP.of_` (velocity `ofThe` pendulum)) $ express E.velocityIYExpr
 
velocityIYDeriv :: Derivation
velocityIYDeriv = mkDerivName (phraseNP (NP.the (yComp `of_` velocity))) (weave [velocityIYDerivSents, velocityIYDerivEqns])

velocityIYDerivSents :: [Sentence]
velocityIYDerivSents = [velocityDerivSent1, velocityIYDerivSent2, velocityDerivSent3,
                        velocityDerivSent4, velocityDerivSent5]

velocityIYDerivEqns :: [Sentence]
velocityIYDerivEqns = map eS D.velocityIYDerivEqns ++ [eS' velocityIYQD]

velocityIYDerivSent2 :: Sentence
velocityIYDerivSent2 = S "We also know the" +:+ phrase verticalPos

-----------------------
accelerationIXGD :: GenDefn
accelerationIXGD = gdNoRefs (equationalModel' accelerationIXQD) (getUnit acceleration)
           (Just accelerationIXDeriv) "accelerationIX" [{-Notes-}]

accelerationIXQD :: ModelQDef
accelerationIXQD = mkQuantDef' xAccel (the xComp `NP.of_` (acceleration `ofThe` pendulum))
    $ express E.accelerationIXExpr

accelerationIXDeriv :: Derivation
accelerationIXDeriv = mkDerivName (phraseNP (NP.the (xComp `of_` acceleration))) (weave [accelerationIXDerivSents, accelerationIXDerivEqns])

accelerationIXDerivSents :: [Sentence]
accelerationIXDerivSents = [accelerationDerivSent1, accelerationIXDerivSent2, accelerationDerivSent3,
    accelerationDerivSent4, accelerationDerivSent5]

accelerationIXDerivEqns :: [Sentence]
accelerationIXDerivEqns = eS D.accelerationIDerivEqn1 : eS' velocityIXQD :
    map eS [D.accelerationIXDerivEqn3, D.accelerationIXDerivEqn4] ++ [eS' accelerationIXQD]

accelerationDerivSent1, accelerationIXDerivSent2, accelerationDerivSent3,
     accelerationDerivSent4, accelerationDerivSent5 :: Sentence

accelerationDerivSent1 = S "Our" +:+ phrase acceleration +: S "is"
accelerationIXDerivSent2 = S "Earlier" `sC` S "we found the" +:+ phrase horizontalVel +:+ S "to be"
accelerationDerivSent3 = S "Applying this to our equation for" +:+ phrase acceleration
accelerationDerivSent4 = S "By the product and chain rules, we find"
accelerationDerivSent5 = S "Simplifying,"

-----------------------
accelerationIYGD :: GenDefn
accelerationIYGD = gdNoRefs (equationalModel' accelerationIYQD) (getUnit acceleration)
           (Just accelerationIYDeriv) "accelerationIY" [{-Notes-}]

accelerationIYQD :: ModelQDef
accelerationIYQD = mkQuantDef' yAccel (the yComp `NP.of_` (acceleration `ofThe` pendulum)) $ express E.accelerationIYExpr

accelerationIYDeriv :: Derivation
accelerationIYDeriv = mkDerivName (phraseNP (NP.the (yComp `of_` acceleration))) (weave [accelerationIYDerivSents, accelerationIYDerivEqns])

accelerationIYDerivSents :: [Sentence]
accelerationIYDerivSents = [accelerationDerivSent1, accelerationIYDerivSent2, accelerationDerivSent3,
    accelerationDerivSent4, accelerationDerivSent5]

accelerationIYDerivEqns :: [Sentence]
accelerationIYDerivEqns = eS D.accelerationIDerivEqn1 : eS' velocityIYQD :
    map eS [D.accelerationIYDerivEqn3, D.accelerationIYDerivEqn4] ++ [eS' accelerationIYQD]

accelerationIYDerivSent2 :: Sentence
accelerationIYDerivSent2 = S "Earlier" `sC` S "we found the" +:+ phrase verticalVel +:+ S "to be"

-------------------------------------Horizontal force acting on the pendulum 
hForceOnPendulumGD :: GenDefn
hForceOnPendulumGD = gdNoRefs (equationalRealmU "hForceOnPendulum" hForceOnPendulumMD)
        (getUnit force) (Just hForceOnPendulumDeriv) "hForceOnPendulum" [{-Notes-}]

hForceOnPendulumMD :: MultiDefn ModelExpr
hForceOnPendulumMD = mkMultiDefnForQuant quant EmptyS defns
    where quant = mkQuant' "force" (horizontalForce `onThe` pendulum)
                    Nothing Real (symbol force) (getUnit force)
          defns = NE.fromList [
                    mkDefiningExpr "hForceOnPendulumViaComponent"
                      [] EmptyS $ express E.hForceOnPendulumViaComponent,
                    mkDefiningExpr "hForceOnPendulumViaAngle"
                      [] EmptyS $ express E.hForceOnPendulumViaAngle
                  ]

hForceOnPendulumDeriv :: Derivation
hForceOnPendulumDeriv = mkDerivName (phraseNP (force `onThe` pendulum)) [eS' hForceOnPendulumMD]

----------------------------------------Vertical force acting on the pendulum 
vForceOnPendulumGD :: GenDefn
vForceOnPendulumGD = gdNoRefs (equationalRealmU "vForceOnPendulum" vForceOnPendulumMD)
        (getUnit force) (Just vForceOnPendulumDeriv) "vForceOnPendulum" [{-Notes-}]

vForceOnPendulumMD :: MultiDefn ModelExpr
vForceOnPendulumMD = mkMultiDefnForQuant quant EmptyS defns
    where quant = mkQuant' "force" (verticalForce `onThe` pendulum)
                    Nothing Real (symbol force) (getUnit force)
          defns = NE.fromList [
                    mkDefiningExpr "vForceOnPendulumViaComponent"
                      [] EmptyS $ express E.vForceOnPendulumViaComponent,
                    mkDefiningExpr "vForceOnPendulumViaAngle"    
                      [] EmptyS $ express E.vForceOnPendulumViaAngle
                  ]

vForceOnPendulumDeriv :: Derivation
vForceOnPendulumDeriv = mkDerivName (phraseNP (force `onThe` pendulum)) [eS' vForceOnPendulumMD]

--------------------------------------Angular Frequency Of Pendulum

angFrequencyGD :: GenDefn
angFrequencyGD = gdNoRefs (equationalModelU "angFrequencyGD" angFrequencyQD) (getUnit angularFrequency)
           (Just angFrequencyDeriv) "angFrequencyGD" [angFrequencyGDNotes]

angFrequencyQD :: ModelQDef
angFrequencyQD = mkQuantDef' angularFrequency (angularFrequency `the_ofThe` pendulum) $ express E.angFrequencyExpr

angFrequencyDeriv :: Derivation
angFrequencyDeriv = mkDerivName (phraseNP (angularFrequency `the_ofThe` pendulum)) (weave [angFrequencyDerivSents, map eS D.angFrequencyDerivEqns])


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

periodPendQD :: ModelQDef
periodPendQD = mkQuantDef' period (NP.the (period `ofThe` pendulum)) $ express E.periodPendExpr

periodPendDeriv :: Derivation
periodPendDeriv = mkDerivName (phraseNP (NP.the (period `ofThe` pendulum))) (weave [periodPendDerivSents, map eS D.periodPendDerivEqns])

periodPendDerivSents :: [Sentence]
periodPendDerivSents = [periodPendDerivSent1, periodPendDerivSent2]

periodPendDerivSent1, periodPendDerivSent2 :: Sentence
periodPendDerivSent1 = atStartNP (period `the_ofThe` pendulum) +:+ S "can be defined from the general definition for the" +:+ phrase equation `S.of_`
                namedRef angFrequencyGD (phrase angFrequencyDD)
periodPendDerivSent2 =  S "Therefore from the data definition of the" +:+ phrase equation `S.for` namedRef angFrequencyDD (phrase angFrequencyDD) `sC` S "we have"

periodPendNotes :: Sentence
periodPendNotes = atStartNP (NP.the (frequency `and_` period)) +:+ S "are defined in the data definitions for" +:+ namedRef frequencyDD (phrase frequencyDD) `S.and_`
        namedRef periodSHMDD (phrase periodSHMDD) +:+ S "respectively"
