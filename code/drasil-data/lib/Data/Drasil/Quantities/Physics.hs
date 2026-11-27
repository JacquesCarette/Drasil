-- | Assigns symbols and units (quantities) to physical concepts.
module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Language.Drasil.Display (Symbol(Concat, Atop), Decoration(Delta))
import Language.Drasil.ShortHands
import qualified Language.Drasil.Sentence.Combinators as S
import Language.Drasil.Chunk.Concept.NamedCombinators (combineNINI, inThe, of_)
import Data.Drasil.Concepts.Math (axis, cartesian, point, xComp, xDir, yComp, yDir)
import qualified Data.Drasil.Concepts.Physics as CP (acceleration, angular,
  constAccel, displacement, distance, energy, force,
  impulseV, linear, position, rigidBody, speed, stress, time, velocity)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)
import Drasil.Database (mkUid)

import Data.Drasil.SI_Units (joule, metre, newton, pascal, radian, second, hertz)
import Data.Drasil.Units.Physics (accelU, angAccelU, angVelU, gravConstU,
    impulseU, momtInertU, torqueU, velU)

restitutionCoef :: DefinedQuantityDict
restitutionCoef = quantNoUnit' (mkUid "restitutionCoef") (cn "coefficient of restitution")
  (S "a measure of the restitution of a collision between two objects")
  (const $ sub cC (label "R")) Real

strain, cohesion :: ConceptChunk
strain = cncpt''' (mkUid "strain") (cn' "strain")
  (S "a measure of deformation representing the" +:+ phrase CP.displacement +:+
   S "between particles in the body relative to a reference length")
  --definition of strain used in SSP, can be made clearer
cohesion = cncpt''' (mkUid "cohesion") (cn "cohesion")
  (S "an attractive" +:+ phrase CP.force +:+ S "between adjacent particles that holds the matter together")
-- | Collects all physical quantities defined in this file for easy use in Drasil.
physicscon :: [DefinedQuantityDict]
physicscon = [acceleration, angularAccel, angularDisplacement, angularVelocity,
  chgInVelocity, constAccel, constAccelV, displacement, distance, energy, frequency,
  fSpeed, fVel, force, gravitationalAccel, gravitationalConst, gravitationalMagnitude,
  height, iPos, iSpeed, ixSpeed, iySpeed, iVel, impulseS, impulseV, ixPos, ixVel, iyPos,
  iyVel, kEnergy, linearAccel, linearDisplacement, linearVelocity, momentOfInertia,
  position, potEnergy, pressure, scalarAccel, scalarPos, speed, time, torque, velocity,
  weight, xAccel, xConstAccel, xDist, xPos, xVel, yAccel, yConstAccel, yDist,
  yPos, yVel,momentum, moment, moment2D, fOfGravity, positionVec, tension,
  angularFrequency, period, chgMomentum]

-- * Physical Quantities (With Units)

acceleration, angularAccel, angularDisplacement, angularVelocity, chgInVelocity,
  constAccel, constAccelV, displacement, distance, energy, fSpeed, fVel, force,
  gravitationalAccel, gravitationalConst, gravitationalMagnitude, height, iPos,
  iSpeed, ixSpeed, iySpeed, iVel, impulseS, impulseV, ixPos, ixVel, iyPos, iyVel,
  kEnergy, linearAccel, linearDisplacement, linearVelocity, momentOfInertia, position,
  potEnergy, pressure, scalarAccel, scalarPos, speed, time, torque, velocity, weight,
  xAccel, xConstAccel, xDist, xPos, xVel, yAccel, yConstAccel, yDist, yPos,
  yVel, momentum, moment, moment2D, fOfGravity, positionVec, tension, angularFrequency,
  period, frequency, chgMomentum :: DefinedQuantityDict

acceleration           = dqd CP.acceleration           (Concat [vec lA, label "(", lT, label ")"]) (Vect Real) accelU
angularAccel           = quant (mkUid "angularAcceleration") (combineNINI CP.angular CP.acceleration)
                          (S "the rate of change of angular velocity")
                          lAlpha Real angAccelU
angularDisplacement    = quant (mkUid "angularDisplacement") (combineNINI CP.angular CP.displacement)
                           (S "the angle through which an object moves on a circular path")
                           lTheta Real radian
angularFrequency       = quant (mkUid "angularFrequency") (cn "angular frequency")
                           (S "the frequency of a periodic process, wave system etc, per unit time.")
                           cOmega Real second
angularVelocity        = quant (mkUid "angularVelocity") (combineNINI CP.angular CP.velocity)
                          (S "the rate of change of angular position of a rotating body")
                          lOmega Real angVelU
chgInVelocity          = quant (mkUid "chgInVelocity") (cn desc)
                           (S "the " +:+ S desc +:+ phrase CP.rigidBody)
                           (Atop Delta $ vec lV) Real velU
  where desc = "change in velocity"
constAccel             = dqd CP.constAccel             (sup lA lC)                                 Real        accelU
displacement           = dqd CP.displacement           (vec lU)                                    Real        metre
distance               = dqd CP.distance               lD                                          Real        metre
energy                 = dqd CP.energy                 cE                                          Real        joule
force                  = dqd CP.force                  (vec cF)                                    (Vect Real) newton
frequency              = quant (mkUid "frequency") (cn' "frequency")
                           (S "the number of occurrences of a repeating event per unit of time")
                           lF Real hertz
gravitationalAccel     = quant (mkUid "gravitationalAccel") (cn "gravitational acceleration")
                           (S "the approximate acceleration due to gravity on Earth at sea level")
                           (vec lG) Real accelU
gravitationalConst     = quant (mkUid "gravitationalConst") (cn "gravitational constant")
                           (S "the empirical physical constant used to show the force between two objects caused by gravity")
                           cG Real gravConstU
gravitationalMagnitude = quant (mkUid "gravitationalMagnitude") (cn "magnitude of gravitational acceleration")
                           (S "the magnitude of the approximate acceleration due to gravity on Earth at sea level")
                           lG Real accelU
height                 = quant (mkUid "height") (cn' "height")
                           (S "the" +:+ phrase distance +:+ S "above a reference point for a point of interest")
                           lH Real metre
impulseS               = quant (mkUid "impulseS") (cn "impulse (scalar)")
                           (S "a force acting briefly on a body and producing a finite change of momentum")
                           lJ Real impulseU
impulseV               = dqd CP.impulseV               (vec cJ)                                    Real        impulseU
kEnergy                = quant (mkUid "kEnergy") (cn "kinetic energy")
                           (S "measure" `S.the_ofThe` phrase CP.energy +:+ S "a body possesses due to its motion")
                           (cK <> cE) Real joule
linearAccel            = quant (mkUid "linearAcceleration") (combineNINI CP.linear CP.acceleration)
                           (S "the rate of change of velocity without a change in direction")
                           (Concat [lA, label "(", lT, label ")"]) Real accelU
linearDisplacement     = quant (mkUid "linearDisplacement") (combineNINI CP.linear CP.displacement)
                           (S "movement in one direction along a single axis")
                           (Concat [lU, label "(", lT, label ")"]) Real metre
linearVelocity         = quant (mkUid "linearVelocity") (combineNINI CP.linear CP.velocity)
                           (S "the speed of a moving object, dependent on the perspective taken")
                           (Concat [lV, label "(", lT, label ")"]) Real velU
momentOfInertia        = quant (mkUid "momentOfInertia") (cn "moment of inertia")
                           (S "a quantity expressing a body's tendency to resist angular acceleration")
                           (vec cI) Real momtInertU
chgMomentum            = quant (mkUid "chgMomentum") (cn' "change in momentum")
                           (S "The rate of change of a body's" +:+ phrase CP.impulseV)
                           (cDelta <> vec cP) Real impulseU

momentum               = quant (mkUid "momentum") (cn "momentum")
                           (S "the quantity of motion" `S.of_` S "a moving body, measured as a product" `S.of_`
                             (phrase QPP.mass `S.and_` phrase CP.velocity))
                           (vec cP) Real impulseU
moment                 = quant (mkUid "moment") (cn' "moment")
                           (S "A measure of the tendency of a body to rotate about a specific" +:+ phrase point `S.or_` phrase axis)
                           (vec cM) Real torqueU
moment2D               = quant (mkUid "moment2D") (cn' "moment")
                           (S "A measure of the tendency of a body to rotate about a specific" +:+ phrase point `S.or_` phrase axis)
                           cM Real torqueU
-- FIXME: moment2D should eventually be a specialization of moment, not separately defined
period                 = quant (mkUid "period") (cn' "period")
                           (S "the" +:+ phrase CP.time +:+ S "required for one complete cycle of vibration to pass a given point.")
                           cT Real second
position               = dqd CP.position               (Concat [vec lP, label "(", lT, label ")"]) Real        metre
positionVec            = quant (mkUid "positionVec") (cn' "position vector")
                           (S "a vector from the origin" `S.ofThe` phrase cartesian +:+ S "defined"
                              `S.toThe` phrase point +:+ S "where the" +:+ phrase CP.force +:+ S "is applied")
                           (vec lR) Real metre
potEnergy              = quant (mkUid "potEnergy") (cn "potential energy")
                           (S "measure" `S.the_ofThe` phrase CP.energy +:+ S "held by an object because of its" +:+ phrase CP.position)
                           (cP <> cE) Real joule
pressure               = quant (mkUid "pressure") (cn' "pressure")
                           (S "a" +:+ phrase CP.force +:+ S "exerted over an area")
                           lP Real pascal
speed                  = dqd CP.speed                  lV                                          Real        velU
scalarAccel            = quant (mkUid "scalarAccel") (cn' "scalar acceleration")
                           (S "magnitude" `S.the_ofThe` phrase CP.acceleration +:+ S "vector")
                           lA Real accelU
scalarPos              = quant (mkUid "scalarPos") (cn' "scalar position")
                           (S "magnitude" `S.the_ofThe` phrase CP.position +:+ S "vector")
                           lP Real metre
tension                = quant (mkUid "tension") (cn' "tension")
                           (S "a" +:+ phrase CP.stress +:+ S "that causes displacement of the body away from its center")
                           (vec cT) Real newton
time                   = dqd CP.time                   lT                                          Real        second
torque                 = quant (mkUid "torque") (cn' "torque")
                           (S "a twisting force that tends to cause rotation")
                           (vec lTau) Real torqueU
velocity               = dqd CP.velocity               (Concat [vec lV, label "(", lT, label ")"]) (Vect Real) velU
weight                 = quant (mkUid "weight") (cn' "weight")
                           (S "the gravitational force acting on an object")
                           cW Real newton
fOfGravity             = quant (mkUid "fOfGravity") (cn "force of gravity")
                           (S "the force exerted by gravity on an object")
                           (sub (vec cF) (vec lG)) Real newton

-- Variants of distance, speed, and scalar acceleration
-- FIXME: Add variants of vector forms?
-- FIXME: Pull out commonalities?

xDist = quant (mkUid "xDist") (CP.distance `inThe` xDir)
  (titleize CP.distance `S.inThe` phrase xDir) (subX lR) Real metre
yDist = quant (mkUid "yDist") (CP.distance `inThe` yDir)
  (titleize CP.distance `S.inThe` phrase yDir) (subY lR) Real metre

iPos = quant (mkUid "iPos") (cn "initial position")
  (S "The" +:+ phrase CP.position +:+ S "at the body's initial point")
  (sup lP initial) Real metre
xPos = quant (mkUid "xPos") (xComp `of_` CP.position)
  (S "The" +:+ NP (xComp `of_` CP.position)) (subX lP) Real metre
yPos = quant (mkUid "yPos") (yComp `of_` CP.position)
  (S "The" +:+ NP (yComp `of_` CP.position)) (subY lP) Real metre

ixPos = quant (mkUid "ixPos") (xComp `of_` iPos)
  (S "The" +:+ NP (xComp `of_` iPos)) (sup (subX lP) initial) Real metre
iyPos = quant (mkUid "iyPos") (yComp `of_` iPos)
  (S "The" +:+ NP (yComp `of_` iPos)) (sup (subY lP) initial) Real metre

fSpeed = quant (mkUid "fSpeed") (cn "final speed")
  (S "The" +:+ phrase CP.speed +:+ S "at the body's final point") (sup lV final) Real velU
iSpeed = quant (mkUid "iSpeed") (cn "initial speed")
  (S "The" +:+ phrase CP.speed +:+ S "at the body's initial point")
  (sup lV initial) Real velU

ixSpeed = quant (mkUid "ixSpeed") (xComp `of_` iSpeed)
  (S "The" +:+ NP (xComp `of_` iSpeed)) (sub lU initial) Real velU -- temporary use for avoiding having too many superscripts
iySpeed = quant (mkUid "iySpeed") (yComp `of_` iSpeed)
  (S "The" +:+ NP (yComp `of_` iSpeed)) (sub lW initial) Real velU -- temporary use for avoiding having too many superscripts

fVel = quant (mkUid "fVel") (cn "final velocity")
  (S "The" +:+ phrase CP.velocity +:+ S "at the body's final point") (sup (vec lV) final) Real velU
iVel = quant (mkUid "iVel") (cn "initial velocity")
  (S "The" +:+ phrase CP.velocity +:+ S "at the body's initial point")
  (sup (vec lV) initial) Real velU
xVel = quant (mkUid "xVel") (xComp `of_` CP.velocity)
  (S "The" +:+ NP (xComp `of_` CP.velocity)) (subX lV) Real velU
yVel = quant (mkUid "yVel") (yComp `of_` CP.velocity)
  (S "The" +:+ NP (yComp `of_` CP.velocity)) (subY lV) Real velU

ixVel = quant (mkUid "ixVel") (xComp `of_` iVel)
  (S "The" +:+ NP (xComp `of_` iVel)) (sup (subX lV) initial) Real velU
iyVel = quant (mkUid "iyVel") (yComp `of_` iVel)
  (S "The" +:+ NP (yComp `of_` iVel)) (sup (subY lV) initial) Real velU

xAccel = quant (mkUid "xScalAcc") (xComp `of_` CP.acceleration)
  (S "The" +:+ NP (xComp `of_` CP.acceleration)) (subX lA) Real accelU
yAccel = quant (mkUid "yScalAcc") (yComp `of_` CP.acceleration)
  (S "The" +:+ NP (yComp `of_` CP.acceleration)) (subY lA) Real accelU

constAccelV = quant (mkUid "constAccelV") (cn "constant acceleration vector")
  (S "The" +:+ phrase CP.constAccel +:+ S "vector") (sup (vec lA) constant) Real accelU
xConstAccel = quant (mkUid "xConstAccel") (xComp `of_` CP.constAccel)
  (S "The" +:+ NP (xComp `of_` CP.constAccel)) (sup (subX lA) constant) Real accelU
yConstAccel = quant (mkUid "yConstAccel") (yComp `of_` CP.constAccel)
  (S "The" +:+ NP (yComp `of_` CP.constAccel)) (sup (subY lA) constant) Real accelU

constant, final, initial :: Symbol
constant = label "c"
final    = label "f"
initial  = label "i"

-- * Helpers

-- Helpers for common modifiers
subMax, subMin, subX, subY, subZ, supMax, supMin :: Symbol -> Symbol
subMax s = sub s (label "max")
subMin s = sub s (label "min")
subX   s = sub s (label "x")
subY   s = sub s (label "y")
subZ   s = sub s (label "z")
supMax s = sup s (label "max")
supMin s = sup s (label "min")

-- * Constants

gravitationalConstValue :: ConstQDef
gravitationalConstValue = mkQuantDef gravitationalConst (dbl 6.6743E-11)
--(dbl 6.673 * 10E-11)
--(dbl 0.00000000006673)
gravitationalAccelConst :: ConstQDef
gravitationalAccelConst = mkQuantDef gravitationalMagnitude (dbl 9.8)
