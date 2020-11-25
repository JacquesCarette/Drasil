module Drasil.DblPendulum.GenDefs (genDefns, velocityIXGD, velocityIYGD,
         accelerationIXGD, accelerationIYGD, hForceOnPendulumGD, vForceOnPendulumGD, angFrequencyGD) where

import Prelude hiding (cos, sin, sqrt)
import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs)
import Utils.Drasil

-- import Data.Drasil.Concepts.Documentation (coordinate, symbol_)
import Data.Drasil.Concepts.Math (xComp, yComp)
import Data.Drasil.Quantities.Physics(velocity, angularVelocity, xVel, yVel,
    angularAccel, xAccel, yAccel, acceleration, force, tension, gravitationalAccel,
    angularFrequency, torque, momentOfInertia, angularDisplacement, time, momentOfInertia)
import Data.Drasil.Concepts.Physics(pendulum, weight)
import Data.Drasil.Quantities.PhysicalProperties(mass)
import Drasil.DblPendulum.TMods(newtonSLR)
import Data.Drasil.Concepts.Math (equation)

-- import Drasil.Projectile.Assumptions (cartSyst, constAccel, pointMass, timeStartZero, twoDMotion)
import Drasil.DblPendulum.Unitals (lenRod, pendDisplacementAngle)

genDefns :: [GenDefn]
genDefns = [velocityIXGD, velocityIYGD, accelerationIXGD, accelerationIYGD,
       hForceOnPendulumGD, vForceOnPendulumGD, angFrequencyGD] 


-- ----------
velocityIXGD :: GenDefn
velocityIXGD = gdNoRefs velocityIXRC (getUnit velocity)
           (Just velocityIXDeriv) "velocityIX" [{-Notes-}]

velocityIXRC :: RelationConcept
velocityIXRC =  makeRC "velocityIXRC" (nounPhraseSent $ foldlSent_ 
            [ phrase xComp `sOf` phrase velocity `ofThe` phrase pendulum])
            EmptyS velocityIXRel

velocityIXRel :: Relation             
velocityIXRel = sy xVel $= sy angularVelocity * sy lenRod * cos (sy pendDisplacementAngle)

velocityIXDeriv :: Derivation
velocityIXDeriv = mkDerivName (phrase xComp +:+ phrase velocity) [ E velocityIXRel]



---------------------
velocityIYGD :: GenDefn
velocityIYGD = gdNoRefs velocityIYRC (getUnit velocity)
           (Just velocityIYDeriv) "velocityIY" [{-Notes-}]

velocityIYRC :: RelationConcept
velocityIYRC = makeRC "velocityIYRC" (nounPhraseSent $ foldlSent_ 
            [ phrase yComp `sOf` phrase velocity `ofThe` phrase pendulum]) EmptyS velocityIYRel
 
velocityIYRel :: Relation             
velocityIYRel = sy yVel $= sy angularVelocity * sy lenRod * cos (sy pendDisplacementAngle)

velocityIYDeriv :: Derivation
velocityIYDeriv = mkDerivName (phrase yComp +:+ phrase velocity) [ E velocityIYRel]

-----------------------
accelerationIXGD :: GenDefn
accelerationIXGD = gdNoRefs accelerationIXRC (getUnit acceleration)
           (Just accelerationIXDeriv) "accelerationIX" [{-Notes-}]

accelerationIXRC :: RelationConcept
accelerationIXRC = makeRC "accelerationIXRC" (nounPhraseSent $ foldlSent_ 
            [ phrase xComp `sOf` phrase acceleration `ofThe` phrase pendulum]) EmptyS accelerationIXRel
 
accelerationIXRel :: Relation             
accelerationIXRel = sy xAccel $= negate (sy angularVelocity * sy lenRod * sin (sy pendDisplacementAngle))
                    + sy angularAccel * sy lenRod * cos (sy pendDisplacementAngle)

accelerationIXDeriv :: Derivation
accelerationIXDeriv = mkDerivName (phrase xComp +:+ phrase acceleration) [ E accelerationIXRel]

-----------------------
accelerationIYGD :: GenDefn
accelerationIYGD = gdNoRefs accelerationIYRC (getUnit acceleration)
           (Just accelerationIYDeriv) "accelerationIY" [{-Notes-}]

accelerationIYRC :: RelationConcept
accelerationIYRC = makeRC "accelerationIYRC" (nounPhraseSent $ foldlSent_ 
            [ phrase yComp `sOf` phrase acceleration `ofThe` phrase pendulum]) EmptyS accelerationIYRel
 
accelerationIYRel :: Relation             
accelerationIYRel = sy yAccel $= (sy angularVelocity * sy lenRod * cos (sy pendDisplacementAngle))
                    + sy angularAccel * sy lenRod * sin (sy pendDisplacementAngle)

accelerationIYDeriv :: Derivation
accelerationIYDeriv = mkDerivName (phrase yComp +:+ phrase acceleration) [ E accelerationIYRel]

-------------------------------------Horizontal force acting on the pendulum 
hForceOnPendulumGD :: GenDefn
hForceOnPendulumGD = gdNoRefs hForceOnPendulumRC (getUnit force)
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
vForceOnPendulumGD = gdNoRefs vForceOnPendulumRC (getUnit force)
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
angFrequencyGD = gdNoRefs angFrequencyRC (getUnit angularFrequency)
           (Just angFrequencyDeriv) "angFrequencyGD" [angFrequencyGDNotes]

angFrequencyRC :: RelationConcept
angFrequencyRC = makeRC "angFrequencyRC" (nounPhraseSent $ foldlSent_ 
            [ S "vertical", phrase force, S "on the", phrase pendulum]) EmptyS angFrequencyRel
 
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

                                 

angFrequencyDerivSent1 = foldlSentCol [S "Consider the", phrase torque `sC` S "on a", phrase pendulum, definedIn'' newtonSLR `sC`
                  S "the", phrase force, S "providing the restoring" +:+ phrase torque `sIs` S "the component of the",
                  phrase weight `ofThe` phrase pendulum, S "bob that acts along the arc length" +:+
                  S "The", phrase torque `isThe` S "length" `ofThe` S "string", ch lenRod +:+ S "multiplied by the component"
                  `ofThe` S "net", phrase force +:+ S "that is perpendicular to the radius" `ofThe` S "arc." +:+
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
angFrequencyGDNotes = S "The" +:+ phrase torque `sIs` definedIn'' newtonSLR
 

                                       
-- But note that for small angles
--  (less than 15 degrees), sinθ and θ differ by less than 1%, so we 
--  can use the small angle approximation sinθ≈θ. The angle θ describes
--   the position of the pendulum. Using the small angle approximation 
--   gives an approximate solution for small angles,
--                                        