module Drasil.DblPendulum.GenDefs (genDefns, velocityIXGD, velocityIYGD,
         accelerationIXGD, accelerationIYGD, hForceOnPendulumGD, vForceOnPendulumGD,
         angFrequencyGD, periodPend) where

import Prelude hiding (cos, sin, sqrt)
import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs, ModelKinds (OthModel))
import Utils.Drasil

-- import Data.Drasil.Concepts.Documentation (coordinate, symbol_)
import Data.Drasil.Concepts.Math (xComp, yComp, equation)
import Data.Drasil.Quantities.Physics(ixPos, iyPos, velocity, angularVelocity, xVel, yVel,
    angularAccel, xAccel, yAccel, acceleration, force, tension, gravitationalAccel,
    angularFrequency, torque, momentOfInertia, angularDisplacement, time,
    momentOfInertia, period, frequency, position)
import Data.Drasil.Concepts.Physics(pendulum, weight)
import Data.Drasil.Quantities.PhysicalProperties(mass)
import Data.Drasil.Theories.Physics(newtonSLR)
import Drasil.DblPendulum.DataDefs(frequencyDD, periodSHMDD, angFrequencyDD)
import qualified Data.Drasil.Quantities.Math as QM (pi_)

-- import Drasil.Projectile.Assumptions (cartSyst, constAccel, pointMass, timeStartZero, twoDMotion)
import Drasil.DblPendulum.Unitals (lenRod, pendDisplacementAngle)

genDefns :: [GenDefn]
genDefns = [velocityIXGD, velocityIYGD, accelerationIXGD, accelerationIYGD,
       hForceOnPendulumGD, vForceOnPendulumGD, angFrequencyGD, periodPend] 


-- ----------
velocityIXGD :: GenDefn
velocityIXGD = gdNoRefs (OthModel velocityIXRC) (getUnit velocity)
           (Just velocityIXDeriv) "velocityIX" [{-Notes-}]

velocityIXRC :: RelationConcept
velocityIXRC =  makeRC "velocityIXRC" (nounPhraseSent $ foldlSent_ 
            [ phrase xComp `sOf` phrase velocity `the_ofThe` phrase pendulum])
            EmptyS velocityIXRel

velocityIXRel :: Relation             
velocityIXRel = sy xVel $= sy angularVelocity * sy lenRod * cos (sy pendDisplacementAngle)

velocityIXDeriv :: Derivation
velocityIXDeriv = mkDerivName (phrase xComp +:+ phrase velocity) (weave [velocityIXDerivSents, map E velocityIXDerivEqns])

velocityIXDerivSents :: [Sentence]
velocityIXDerivSents = [velocityIDerivSent1,velocityIXDerivSent2,velocityIXDerivSent3,velocityIXDerivSent4,
                            velocityIXDerivSent5]

velocityIXDerivEqns :: [Expr]
velocityIXDerivEqns = [velocityIDerivEqn1,velocityIXDerivEqn2,velocityIXDerivEqn3,
                            velocityIXDerivEqn4,velocityIXRel]

velocityIDerivSent1,velocityIXDerivSent2,velocityIXDerivSent3,velocityIXDerivSent4,velocityIXDerivSent5 :: Sentence
velocityIDerivEqn1,velocityIXDerivEqn2,velocityIXDerivEqn3,velocityIXDerivEqn4 :: Expr

{-velocityIDerivSent1 = S "Using the formula for arc length" `sC` (phrase position `the_ofThe` phrase pendulum) +: S "may be defined as"
velocityIDerivEqn1 = sy position $= sy pendDisplacementAngle * sy lenRod
velocityIDerivSent2 = S "Then" `sC` S "the" +:+ phrase velocity `ofThe` phrase pendulum +:+ S "at a given time will be"
velocityIDerivEqn2 = sy velocity $= deriv (sy position) time 
velocityIDerivSent3 = S "Applying this, we find"
velocityIDerivEqn3 = sy velocity $= deriv (sy pendDisplacementAngle * sy lenRod) time
velocityIDerivSent4 = E (sy lenRod) `sIs` S "constant with respect to time, so"
velocityIDerivEqn4 = sy velocity $= sy lenRod * deriv (sy pendDisplacementAngle) time
velocityIDerivSent5 = S "We also know that"
velocityIDerivEqn5 = sy angularVelocity $= deriv (sy pendDisplacementAngle) time
velocityIDerivSent6 = S "So,"
velocityIDerivEqn6 = sy velocity $= sy angularVelocity * sy lenRod
velocityIXDerivSent7 = S "For the x-component of" +: phrase velocity 
velocityIYDerivSent7 = S "For the y-component of" +: phrase velocity-}

velocityIDerivSent1 = S "At a given point in time, velocity may be defined as"
velocityIDerivEqn1 = sy velocity $= deriv (sy position) time
velocityIXDerivSent2 = S "We also know that at a given horizontal position,"
velocityIXDerivEqn2 = sy ixPos $= sy lenRod * sin (sy pendDisplacementAngle)
velocityIXDerivSent3 = S "Applying this,"
velocityIXDerivEqn3 = sy xVel $= deriv (sy lenRod * sin (sy pendDisplacementAngle)) time
velocityIXDerivSent4 = E (sy lenRod) `sIs` S "constant with respect to time, so"
velocityIXDerivEqn4 = sy xVel $= sy lenRod * deriv (sin (sy pendDisplacementAngle)) time
velocityIXDerivSent5 = S "Therefore,"
--velocityIXDerivEqn5 = sy xVel $= sy lenRod * cos (sy pendDisplacementAngle)

---------------------
velocityIYGD :: GenDefn
velocityIYGD = gdNoRefs (OthModel velocityIYRC) (getUnit velocity)
           (Just velocityIYDeriv) "velocityIY" [{-Notes-}]

velocityIYRC :: RelationConcept
velocityIYRC = makeRC "velocityIYRC" (nounPhraseSent $ foldlSent_ 
            [ phrase yComp `sOf` phrase velocity `the_ofThe` phrase pendulum]) EmptyS velocityIYRel
 
velocityIYRel :: Relation             
velocityIYRel = sy yVel $= negate (sy angularVelocity * sy lenRod * sin (sy pendDisplacementAngle))

velocityIYDeriv :: Derivation
velocityIYDeriv = mkDerivName (phrase yComp +:+ phrase velocity) (weave [velocityIYDerivSents, map E velocityIYDerivEqns])

velocityIYDerivSents :: [Sentence]
velocityIYDerivSents = [velocityIDerivSent1,velocityIYDerivSent2,velocityIYDerivSent3,velocityIYDerivSent4,
                            velocityIYDerivSent5,velocityIYDerivSent5]

velocityIYDerivEqns :: [Expr]
velocityIYDerivEqns = [velocityIDerivEqn1,velocityIYDerivEqn2,velocityIYDerivEqn3,
                            velocityIYDerivEqn4,velocityIYRel]

velocityIYDerivSent2,velocityIYDerivSent3,velocityIYDerivSent4,velocityIYDerivSent5 :: Sentence
velocityIYDerivEqn2,velocityIYDerivEqn3,velocityIYDerivEqn4 :: Expr

velocityIYDerivSent2 = S "We also know that at a given horizontal position,"
velocityIYDerivEqn2 = sy iyPos $= sy lenRod * cos (sy pendDisplacementAngle)
velocityIYDerivSent3 = S "Applying this again,"
velocityIYDerivEqn3 = sy yVel $= deriv (sy lenRod * cos (sy pendDisplacementAngle)) time
velocityIYDerivSent4 = E (sy lenRod) `sIs` S "constant with respect to time, so"
velocityIYDerivEqn4 = sy yVel $= sy lenRod * deriv (cos (sy pendDisplacementAngle)) time
velocityIYDerivSent5 = S "Therefore,"
--velocityIYDerivEqn5 = sy yVel $= sy angularVelocity * sy lenRod * sin (sy pendDisplacementAngle)

-----------------------
accelerationIXGD :: GenDefn
accelerationIXGD = gdNoRefs (OthModel accelerationIXRC) (getUnit acceleration)
           (Just accelerationIXDeriv) "accelerationIX" [{-Notes-}]

accelerationIXRC :: RelationConcept
accelerationIXRC = makeRC "accelerationIXRC" (nounPhraseSent $ foldlSent_ 
            [ phrase xComp `sOf` phrase acceleration `the_ofThe` phrase pendulum]) EmptyS accelerationIXRel
 
accelerationIXRel :: Relation             
accelerationIXRel = sy xAccel $= negate (sy angularVelocity * sy lenRod * sin (sy pendDisplacementAngle))
                    + sy angularAccel * sy lenRod * cos (sy pendDisplacementAngle)

accelerationIXDeriv :: Derivation
accelerationIXDeriv = mkDerivName (phrase xComp +:+ phrase acceleration) [ E accelerationIXRel]

-----------------------
accelerationIYGD :: GenDefn
accelerationIYGD = gdNoRefs (OthModel accelerationIYRC) (getUnit acceleration)
           (Just accelerationIYDeriv) "accelerationIY" [{-Notes-}]

accelerationIYRC :: RelationConcept
accelerationIYRC = makeRC "accelerationIYRC" (nounPhraseSent $ foldlSent_ 
            [ phrase yComp `sOf` phrase acceleration `the_ofThe` phrase pendulum]) EmptyS accelerationIYRel
 
accelerationIYRel :: Relation             
accelerationIYRel = sy yAccel $= (sy angularVelocity * sy lenRod * cos (sy pendDisplacementAngle))
                    + sy angularAccel * sy lenRod * sin (sy pendDisplacementAngle)

accelerationIYDeriv :: Derivation
accelerationIYDeriv = mkDerivName (phrase yComp +:+ phrase acceleration) [ E accelerationIYRel]

-------------------------------------Horizontal force acting on the pendulum 
hForceOnPendulumGD :: GenDefn
hForceOnPendulumGD = gdNoRefs (OthModel hForceOnPendulumRC) (getUnit force)
           (Just hForceOnPendulumDeriv) "hForceOnPendulum" [{-Notes-}]

hForceOnPendulumRC :: RelationConcept
hForceOnPendulumRC = makeRC "hForceOnPendulumRC" (nounPhraseSent $ foldlSent_ 
            [ S "horizontal", phrase force, S "on the", phrase pendulum]) EmptyS hForceOnPendulumRel
 
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
            [ S "vertical", phrase force, S "on the", phrase pendulum]) EmptyS vForceOnPendulumRel
 
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
                  S "The", phrase force, S "providing the restoring" +:+ phrase torque `sIs`(S "component of" +:+
                  phrase weight `the_ofThe` phrase pendulum) +:+. S "bob that acts along the arc length",
                  (phrase torque `isThe` S "length") `the_ofThe'` S "string", ch lenRod +:+ S "multiplied by", S "component"
                  `the_ofThe` S "net", phrase force +:+ S "that is perpendicular to", S "radius" `the_ofThe` S "arc." +:+
                  S "The minus sign indicates the" +:+ phrase torque +:+ S "acts in the opposite direction of the", phrase angularDisplacement]


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
                  S "for simple harmonic motion the solution is easy to find." +:+ S " The" +:+ phrase angularFrequency
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

