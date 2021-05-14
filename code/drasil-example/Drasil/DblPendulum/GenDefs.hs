{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPendulum.GenDefs (genDefns, velocityIXGD, velocityIYGD,
         accelerationIXGD, accelerationIYGD, hForceOnPendulumGD, vForceOnPendulumGD,
         angFrequencyGD, periodPend) where

import Prelude hiding (cos, sin, sqrt)
import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs, ModelKinds (OthModel, EquationalModel))
import Utils.Drasil

-- import Data.Drasil.Concepts.Documentation (coordinate, symbol_)
import Data.Drasil.Concepts.Math (xComp, yComp, equation, component)
import Data.Drasil.Quantities.Physics(xPos, yPos, velocity, angularVelocity, xVel, yVel,
    angularAccel, xAccel, yAccel, acceleration, force, tension, gravitationalAccel,
    angularFrequency, torque, momentOfInertia, angularDisplacement, time,
    momentOfInertia, period, frequency, position)
import Data.Drasil.Concepts.Physics(pendulum, weight, shm)
import Data.Drasil.Quantities.PhysicalProperties(mass)
import Data.Drasil.Theories.Physics(newtonSLR)
import Drasil.DblPendulum.DataDefs(frequencyDD, periodSHMDD, angFrequencyDD)
import qualified Data.Drasil.Quantities.Math as QM (pi_)

-- import Drasil.Projectile.Assumptions (cartSyst, constAccel, pointMass, timeStartZero, twoDMotion)
import Drasil.DblPendulum.Unitals (lenRod, pendDisplacementAngle)
import Drasil.DblPendulum.Concepts (horizontal, vertical, arcLen)

genDefns :: [GenDefn]
genDefns = [velocityIXGD, velocityIYGD, accelerationIXGD, accelerationIYGD,
       hForceOnPendulumGD, vForceOnPendulumGD, angFrequencyGD, periodPend] 


-- ----------
velocityIXGD :: GenDefn
velocityIXGD = gdNoRefs (EquationalModel velocityIXQD) (getUnit velocity)
           (Just velocityIXDeriv) "velocityIX" [{-Notes-}]

velocityIXQD :: QDefinition
velocityIXQD = mkQuantDef' xVel (nounPhraseSent $ foldlSent_ 
    [phrase xComp `sOf` phrase velocity `the_ofThe` phrase pendulum])
    velocityIXExpr

velocityIXExpr :: Expr             
velocityIXExpr = sy angularVelocity * sy lenRod * cos (sy pendDisplacementAngle)

velocityIXDeriv :: Derivation
velocityIXDeriv = mkDerivName (phrase xComp +:+ phrase velocity) (weave [velocityIXDerivSents, map E velocityIXDerivEqns])

velocityIXDerivSents :: [Sentence]
velocityIXDerivSents = [velocityIDerivSent1,velocityIXDerivSent2,velocityIXDerivSent3,velocityIXDerivSent4,
                            velocityIXDerivSent5]

velocityIXDerivEqns :: [Expr]
velocityIXDerivEqns = [velocityIDerivEqn1,velocityIXDerivEqn2,velocityIXDerivEqn3,
                            velocityIXDerivEqn4, relat velocityIXQD]

velocityIDerivSent1,velocityIXDerivSent2,velocityIXDerivSent3,velocityIXDerivSent4,velocityIXDerivSent5 :: Sentence
velocityIDerivEqn1,velocityIXDerivEqn2,velocityIXDerivEqn3,velocityIXDerivEqn4 :: Expr

velocityIDerivSent1 = S "At a given point in time" `sC` phrase velocity +:+ S "may be defined as"
velocityIDerivEqn1 = sy velocity $= deriv (sy position) time
velocityIXDerivSent2 = S "We also know the" +:+ phrase horizontal +:+ phrase position
velocityIXDerivEqn2 = sy xPos $= sy lenRod * sin (sy pendDisplacementAngle) 
velocityIXDerivSent3 = S "Applying this,"
velocityIXDerivEqn3 = sy xVel $= deriv (sy lenRod * sin (sy pendDisplacementAngle)) time
velocityIXDerivSent4 = E (sy lenRod) `sIs` S "constant with respect to time, so"
velocityIXDerivEqn4 = sy xVel $= sy lenRod * deriv (sin (sy pendDisplacementAngle)) time
velocityIXDerivSent5 = S "Therefore, using the chain rule,"

---------------------
velocityIYGD :: GenDefn
velocityIYGD = gdNoRefs (EquationalModel velocityIYQD) (getUnit velocity)
           (Just velocityIYDeriv) "velocityIY" [{-Notes-}]

velocityIYQD :: QDefinition
velocityIYQD = mkQuantDef' yVel (nounPhraseSent $ foldlSent_
    [phrase yComp `sOf` phrase velocity `the_ofThe` phrase pendulum]) velocityIYExpr
 
velocityIYExpr :: Expr             
velocityIYExpr = sy angularVelocity * sy lenRod * sin (sy pendDisplacementAngle)

velocityIYDeriv :: Derivation
velocityIYDeriv = mkDerivName (phrase yComp +:+ phrase velocity) (weave [velocityIYDerivSents, map E velocityIYDerivEqns])

velocityIYDerivSents :: [Sentence]
velocityIYDerivSents = [velocityIDerivSent1, velocityIYDerivSent2,
                        velocityIYDerivSent3, velocityIYDerivSent4,
                        velocityIYDerivSent5, velocityIYDerivSent5]

velocityIYDerivEqns :: [Expr]
velocityIYDerivEqns = [velocityIDerivEqn1, velocityIYDerivEqn2,
                       velocityIYDerivEqn3, velocityIYDerivEqn4,
                       relat velocityIYQD]

velocityIYDerivSent2,velocityIYDerivSent3,velocityIYDerivSent4,velocityIYDerivSent5 :: Sentence
velocityIYDerivEqn2,velocityIYDerivEqn3,velocityIYDerivEqn4 :: Expr

velocityIYDerivSent2 = S "We also know the" +:+ phrase vertical +:+ phrase position
velocityIYDerivEqn2 = sy yPos $= negate (sy lenRod * cos (sy pendDisplacementAngle)) 
velocityIYDerivSent3 = S "Applying this again,"
velocityIYDerivEqn3 = sy yVel $= negate (deriv (sy lenRod * cos (sy pendDisplacementAngle)) time)
velocityIYDerivSent4 = E (sy lenRod) `sIs` S "constant with respect to time, so"
velocityIYDerivEqn4 = sy yVel $= negate (sy lenRod * deriv (cos (sy pendDisplacementAngle)) time)
velocityIYDerivSent5 = S "Therefore, using the chain rule,"

-----------------------
accelerationIXGD :: GenDefn
accelerationIXGD = gdNoRefs (EquationalModel accelerationIXQD) (getUnit acceleration)
           (Just accelerationIXDeriv) "accelerationIX" [{-Notes-}]

accelerationIXQD :: QDefinition
accelerationIXQD = mkQuantDef' xAccel (nounPhraseSent $ foldlSent_ 
    [phrase xComp `sOf` phrase acceleration `the_ofThe` phrase pendulum]) accelerationIXExpr
 
accelerationIXExpr :: Expr             
accelerationIXExpr = negate (square (sy angularVelocity) * sy lenRod * sin (sy pendDisplacementAngle))
                    + sy angularAccel * sy lenRod * cos (sy pendDisplacementAngle)

accelerationIXDeriv :: Derivation
accelerationIXDeriv = mkDerivName (phrase xComp +:+ phrase acceleration) (weave [accelerationIXDerivSents, map E accelerationIXDerivEqns])

accelerationIXDerivSents :: [Sentence]
accelerationIXDerivSents = [accelerationIDerivSent1, accelerationIXDerivSent2, accelerationIXDerivSent3,
    accelerationIXDerivSent4, accelerationIXDerivSent5]

accelerationIXDerivEqns :: [Expr]
accelerationIXDerivEqns = [accelerationIDerivEqn1, accelerationIXDerivEqn2, accelerationIXDerivEqn3, accelerationIXDerivEqn4, relat accelerationIXQD]

accelerationIDerivSent1, accelerationIXDerivSent2, accelerationIXDerivSent3,
     accelerationIXDerivSent4, accelerationIXDerivSent5 :: Sentence
accelerationIDerivEqn1, accelerationIXDerivEqn2, accelerationIXDerivEqn3, accelerationIXDerivEqn4 :: Expr

accelerationIDerivSent1 = S "Our" +:+ phrase acceleration +: S "is"
accelerationIDerivEqn1 = sy acceleration $= deriv (sy velocity) time 
accelerationIXDerivSent2 = S "Earlier" `sC` S "we found the" +:+ phrase horizontal +:+ phrase velocity +:+ S "to be"
accelerationIXDerivEqn2 = relat velocityIXQD
accelerationIXDerivSent3 = S "Applying this to our equation for" +:+ phrase acceleration
accelerationIXDerivEqn3 = sy xAccel $= deriv (sy angularVelocity * sy lenRod * cos (sy pendDisplacementAngle)) time
accelerationIXDerivSent4 = S "By the product and chain rules, we find"
accelerationIXDerivEqn4 = sy xAccel $= deriv (sy angularVelocity) time * sy lenRod * cos (sy pendDisplacementAngle)
                        - (sy angularVelocity * sy lenRod * sin (sy pendDisplacementAngle) * deriv (sy pendDisplacementAngle) time)
accelerationIXDerivSent5 = S "Simplifying,"

-----------------------
accelerationIYGD :: GenDefn
accelerationIYGD = gdNoRefs (EquationalModel accelerationIYQD) (getUnit acceleration)
           (Just accelerationIYDeriv) "accelerationIY" [{-Notes-}]

accelerationIYQD :: QDefinition
accelerationIYQD = mkQuantDef' yAccel (nounPhraseSent $ foldlSent_
    [phrase yComp `sOf` phrase acceleration `the_ofThe` phrase pendulum]) accelerationIYExpr

accelerationIYExpr :: Expr             
accelerationIYExpr = (square (sy angularVelocity) * sy lenRod * cos (sy pendDisplacementAngle))
                    + sy angularAccel * sy lenRod * sin (sy pendDisplacementAngle)

accelerationIYDeriv :: Derivation
accelerationIYDeriv = mkDerivName (phrase yComp +:+ phrase acceleration) (weave [accelerationIYDerivSents, map E accelerationIYDerivEqns])

accelerationIYDerivSents :: [Sentence]
accelerationIYDerivSents = [accelerationIDerivSent1, accelerationIYDerivSent2, accelerationIYDerivSent3,
    accelerationIYDerivSent4, accelerationIYDerivSent5]

accelerationIYDerivEqns :: [Expr]
accelerationIYDerivEqns = [accelerationIDerivEqn1, accelerationIYDerivEqn2, accelerationIYDerivEqn3, accelerationIYDerivEqn4, relat accelerationIYQD]

accelerationIYDerivSent2, accelerationIYDerivSent3, accelerationIYDerivSent4, 
    accelerationIYDerivSent5 :: Sentence
accelerationIYDerivEqn2, accelerationIYDerivEqn3, accelerationIYDerivEqn4 :: Expr

accelerationIYDerivSent2 = S "Earlier" `sC` S "we found the" +:+ phrase vertical +:+ phrase velocity +:+ S "to be"
accelerationIYDerivEqn2 = relat velocityIYQD
accelerationIYDerivSent3 = S "Applying this to our equation for" +:+ phrase acceleration
accelerationIYDerivEqn3 = sy yAccel $= deriv (sy angularVelocity * sy lenRod * sin (sy pendDisplacementAngle)) time
accelerationIYDerivSent4 = S "By the product and chain rules, we find"
accelerationIYDerivEqn4 = sy yAccel $= deriv (sy angularVelocity) time * sy lenRod * sin (sy pendDisplacementAngle)
                        + sy angularVelocity * sy lenRod * cos (sy pendDisplacementAngle) * deriv (sy pendDisplacementAngle) time
accelerationIYDerivSent5 = S "Simplifying,"

-------------------------------------Horizontal force acting on the pendulum 
hForceOnPendulumGD :: GenDefn
hForceOnPendulumGD = gdNoRefs (OthModel hForceOnPendulumRC) (getUnit force)
           (Just hForceOnPendulumDeriv) "hForceOnPendulum" [{-Notes-}]

hForceOnPendulumRC :: RelationConcept
hForceOnPendulumRC = makeRC "hForceOnPendulumRC" (nounPhraseSent $ foldlSent_ 
            [atStart horizontal, phrase force, S "on the", phrase pendulum]) EmptyS hForceOnPendulumRel
 
hForceOnPendulumRel :: Relation             
hForceOnPendulumRel = sy force $= sy mass * sy xAccel $= negate (sy tension * sin (sy pendDisplacementAngle))
                     

hForceOnPendulumDeriv :: Derivation
hForceOnPendulumDeriv = mkDerivName (phrase force +:+ phrase pendulum) [ E hForceOnPendulumRel]

----------------------------------------Vertical force acting on the pendulum 
vForceOnPendulumGD :: GenDefn
vForceOnPendulumGD = gdNoRefs (OthModel vForceOnPendulumRC) (getUnit force)
           (Just vForceOnPendulumDeriv) "vForceOnPendulum" [{-Notes-}]

vForceOnPendulumRC :: RelationConcept
vForceOnPendulumRC = makeRC "vForceOnPendulumRC" (nounPhraseSent $ foldlSent_ 
            [atStart vertical, phrase force, S "on the", phrase pendulum]) EmptyS vForceOnPendulumRel
 
vForceOnPendulumRel :: Relation             
vForceOnPendulumRel = sy force $= sy mass * sy yAccel $= sy tension * cos (sy pendDisplacementAngle) - sy mass * sy gravitationalAccel

vForceOnPendulumDeriv :: Derivation
vForceOnPendulumDeriv = mkDerivName (phrase force +:+ phrase pendulum) [ E vForceOnPendulumRel]

--------------------------------------Angular Frequency Of Pendulum

angFrequencyGD :: GenDefn
angFrequencyGD = gdNoRefs (OthModel angFrequencyRC) (getUnit angularFrequency)
           (Just angFrequencyDeriv) "angFrequencyGD" [angFrequencyGDNotes]

angFrequencyRC :: RelationConcept
angFrequencyRC = makeRC "angFrequencyRC" (nounPhraseSent $ foldlSent_ 
            [ phrase angularFrequency `the_ofThe` phrase pendulum]) EmptyS angFrequencyRel
 
angFrequencyRel :: Relation             
angFrequencyRel = sy angularFrequency $= sqrt (sy gravitationalAccel / sy lenRod )

angFrequencyDeriv :: Derivation
angFrequencyDeriv = mkDerivName (phrase angularFrequency +:+ phrase pendulum) (weave [angFrequencyDerivSents, map E angFrequencyDerivEqns])


angFrequencyDerivSents :: [Sentence]
angFrequencyDerivSents = [angFrequencyDerivSent1, angFrequencyDerivSent2, angFrequencyDerivSent3,
                      angFrequencyDerivSent4, angFrequencyDerivSent5, angFrequencyDerivSent6, angFrequencyDerivSent7]

angFrequencyDerivSent1, angFrequencyDerivSent2, angFrequencyDerivSent3,
     angFrequencyDerivSent4, angFrequencyDerivSent5, angFrequencyDerivSent6, angFrequencyDerivSent7 :: Sentence

angFrequencyDerivEqns :: [Expr]
angFrequencyDerivEqns = [angFrequencyDerivEqn1, angFrequencyDerivEqn2, angFrequencyDerivEqn3,
                     angFrequencyDerivEqn4, angFrequencyDerivEqn5, angFrequencyDerivEqn6, angFrequencyDerivEqn7]

angFrequencyDerivEqn1, angFrequencyDerivEqn2, angFrequencyDerivEqn3, angFrequencyDerivEqn4,
                   angFrequencyDerivEqn5, angFrequencyDerivEqn6, angFrequencyDerivEqn7 :: Expr

                                 

angFrequencyDerivSent1 = foldlSentCol [S "Consider the", phrase torque, S "on a", phrase pendulum +:+. definedIn'' newtonSLR,
                  S "The", phrase force, S "providing the restoring", phrase torque `sIs` (phrase component `sOf`
                  phrase weight `the_ofThe` phrase pendulum), S "bob that acts along the" +:+. phrase arcLen,
                  (phrase torque `isThe` S "length") `the_ofThe'` S "string", ch lenRod, S "multiplied by", S "component"
                  `the_ofThe` S "net", phrase force, S "that is perpendicular to", S "radius" `the_ofThe` (S "arc" !.),
                  S "The minus sign indicates the", phrase torque, S "acts in the opposite direction of the", phrase angularDisplacement]


angFrequencyDerivEqn1 = sy torque $= negate (sy lenRod) * (sy mass * sy gravitationalAccel * sin (sy pendDisplacementAngle))
angFrequencyDerivSent2 = S "So then"
angFrequencyDerivEqn2 = sy momentOfInertia * sy angularAccel $= negate (sy lenRod) * (sy mass * sy gravitationalAccel * sin (sy pendDisplacementAngle))
angFrequencyDerivSent3 = S "Therefore,"
angFrequencyDerivEqn3 = sy momentOfInertia * deriv (deriv (sy pendDisplacementAngle) time) time $= negate (sy lenRod)
             * sy mass * sy gravitationalAccel * sin (sy pendDisplacementAngle)
angFrequencyDerivSent4 = S "Substituting for" +:+ ch momentOfInertia
angFrequencyDerivEqn4 = sy mass * sy lenRod $^ 2 * deriv (deriv (sy pendDisplacementAngle) time) time $= negate (sy lenRod)
             * sy mass * sy gravitationalAccel * sin (sy pendDisplacementAngle)
angFrequencyDerivSent5 = S "Crossing out" +:+ ch mass `sAnd` ch lenRod +:+ S "we have"
angFrequencyDerivEqn5 = deriv (deriv (sy pendDisplacementAngle) time) time $= negate(sy gravitationalAccel/ sy lenRod) * sin (sy pendDisplacementAngle)
angFrequencyDerivSent6 = S "For small angles, we approximate" +:+ S "sin" +:+ ch pendDisplacementAngle +:+ S "to" +:+ ch pendDisplacementAngle
angFrequencyDerivEqn6 = deriv (deriv (sy pendDisplacementAngle) time) time $= negate(sy gravitationalAccel/ sy lenRod) * sy pendDisplacementAngle
angFrequencyDerivSent7 = S "Because this" +:+ phrase equation `sC` S "has the same form as the" +:+ phrase equation +:+
                  S "for" +:+ phrase shm +:+. S "the solution is easy to find" +:+ S " The" +:+ phrase angularFrequency
angFrequencyDerivEqn7 = sy angularFrequency $= sqrt (sy gravitationalAccel / sy lenRod)
angFrequencyGDNotes :: Sentence
angFrequencyGDNotes = S "The" +:+ phrase torque `sIs` definedIn'' newtonSLR  `sAnd` phrase frequency `sIs` definedIn frequencyDD
 

                                       
 --------------------------------Period of Pendulum Motion 

periodPend :: GenDefn
periodPend = gdNoRefs (OthModel periodPendRC) (getUnit period)
           (Just periodPendDeriv) "periodPend" [periodPendNotes]

periodPendRC :: RelationConcept
periodPendRC = makeRC "periodPendRC" (nounPhraseSent $ foldlSent_ 
            [ S "The", phrase period, S "on the", phrase pendulum]) EmptyS periodPendRel
 
periodPendRel :: Relation             
periodPendRel = sy period $= 2 * sy QM.pi_ * sqrt (sy lenRod/ sy gravitationalAccel)

periodPendDeriv :: Derivation
periodPendDeriv = mkDerivName (phrase period +:+ phrase pendulum) (weave [periodPendDerivSents, map E periodPendDerivEqns])    

periodPendDerivSents :: [Sentence]
periodPendDerivSents = [periodPendDerivSent1, periodPendDerivSent2]

periodPendDerivSent1, periodPendDerivSent2 :: Sentence    

periodPendDerivEqns :: [Expr]
periodPendDerivEqns = [periodPendDerivEqn1, periodPendDerivEqn2]

periodPendDerivEqn1, periodPendDerivEqn2 :: Expr 

periodPendDerivSent1 = phrase period `the_ofThe'` phrase pendulum +:+ S "can be defined from" +:+
                makeRef2S angFrequencyGD +:+ phrase equation
periodPendDerivEqn1 = sy angularFrequency $= sqrt (sy gravitationalAccel / sy lenRod)
periodPendDerivSent2 =  S "Therefore from the" +:+ phrase equation +:+ makeRef2S angFrequencyDD `sC` S "we have"

periodPendDerivEqn2 = sy period $= 2 * sy QM.pi_ * sqrt (sy lenRod/ sy gravitationalAccel)

periodPendNotes :: Sentence
periodPendNotes = S "The" +:+ phrase frequency `sAnd` phrase period +:+ S "are defined in" +:+ makeRef2S frequencyDD +:+
        makeRef2S periodSHMDD +:+ S "respectively"

