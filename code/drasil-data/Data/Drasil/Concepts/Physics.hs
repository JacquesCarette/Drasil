module Data.Drasil.Concepts.Physics where
--This is obviously a bad name, but for now it will do until we come
--  up with a better one.
import Language.Drasil
import Utils.Drasil

import Data.Drasil.IdeaDicts (mathematics, physics)
import Data.Drasil.Concepts.Documentation (property, value)
import Data.Drasil.Concepts.Math (xComp, xDir, yComp, yDir, point, axis, cartesian)
import Control.Lens((^.)) --need for parametrization hack
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)
import Data.Drasil.Citations (dampingSource)
import Data.Drasil.Concepts.Education (mechanics)

physicCon :: [ConceptChunk]
physicCon = [acceleration, angAccel, angDisp, angVelo, angFreq, angular, chgInVelocity,
  cohesion, collision, compression, constAccel, constAccelV, damping,
  displacement, distance, elasticity, energy, fSpeed, fVel, fbd, force,
  friction, gravity, gravitationalAccel, gravitationalConst, height, iPos,
  iSpeed, iVel, impulseS, impulseV, isotropy, ixPos, ixVel, iyPos, iyVel,
  joint, kEnergy, linAccel, linDisp, linVelo, linear, mechEnergy,
  momentOfInertia, position, potEnergy, pressure, restitutionCoef, rectilinear,
  rigidBody, scalarAccel, scalarPos, space, speed, strain, stress, tension,
  time, torque, velocity, weight, xAccel, xConstAccel, xDist, xPos, xVel,
  yAccel, yConstAccel, yDist, yPos, yVel, momentum, moment, fOfGravity, positionVec,
  pendulum, body, kinematics, frequency, period]

physicCon' :: [CI]
physicCon' = [oneD, twoD, threeD]

acceleration, angAccel, angDisp, angVelo, angFreq, angular, chgInVelocity, cohesion,
  collision, compression, constAccel, constAccelV, damping, displacement,
  distance, elasticity, energy, fSpeed, fVel, fbd, force, friction, gravity,
  gravitationalAccel, gravitationalConst, height, iPos, iSpeed, iVel, impulseS,
  impulseV, isotropy, ixPos, ixVel, iyPos, iyVel, joint, kEnergy, linAccel,
  linDisp, linVelo, linear, mechEnergy, momentOfInertia, position, potEnergy,
  pressure, rectilinear, restitutionCoef, rigidBody, scalarAccel, scalarPos,
  space, speed, strain, stress, tension, time, torque, velocity, weight,
  xAccel, xConstAccel, xDist, xPos, xVel, yAccel, yConstAccel, yDist,
  yPos, yVel, momentum, moment, fOfGravity, positionVec, pendulum, body,
  kinematics, frequency, period :: ConceptChunk

oneD, twoD, threeD :: CI
oneD   = commonIdeaWithDict "oneD"   (cn "one-dimensional")   "1D" [mathematics, physics]
twoD   = commonIdeaWithDict "twoD"   (cn "two-dimensional")   "2D" [mathematics, physics]
threeD = commonIdeaWithDict "threeD" (cn "three-dimensional") "3D" [mathematics, physics]

acceleration = dccWDS "acceleration" (cn' "acceleration")
  (S "the rate of change of a body's" +:+ phrase velocity)
angular = dcc "angular" (cn' "angular")
  "denoting physical properties or quantities measured with reference to or by means of an angle"
body = dccWDS "body" (cnIES "body")
  (S "an object with" +:+ phrase QPP.mass)
chgInVelocity = dccWDS "chgInVelocity" (cn "change in velocity")
  (S "the" +:+ phrase chgInVelocity `sOf` S "a" +:+ phrase rigidBody)
collision = dcc "collision" (cn' "collision")
  "an encounter between particles resulting in an exchange or transformation of energy"
cohesion = dccWDS "cohesion" (cn "cohesion")
  (S "an attractive" +:+ phrase force +:+ S "between adjacent particles that holds the matter together")
compression = dccWDS "compression" (cn' "compression")
  (S "a" +:+ phrase stress +:+ S "that causes displacement of the body towards its center")
damping = dccWDS "damping" (pn' "damping")
  $ S "an influence within or upon an oscillatory system that has the effect of reducing," +:+
  S "restricting or preventing its oscillations" +:+ sParen (S "from" +:+ makeRef2S dampingSource)
displacement = dccWDS "displacement" (cn' "displacement")
  (S "the change in" +:+ (position ^. defn))
distance = dcc "distance" (cn' "distance")
  "the interval measured along a path connecting two locations"
elasticity = dcc "elasticity" (cnIES "elasticity")
  "the ratio of the relative velocities of two colliding objects after and before a collision"
energy = dcc "energy" (cn "energy")
  "power derived from the utilization of physical or chemical resources"
fbd = dcc "FBD" (cn' "free body diagram")
  ("a graphical illustration used to visualize the applied forces, movements, and resulting " ++
   "reactions on a body in a steady state condition")
force = dcc "force" (cn' "force")
  "an interaction that tends to produce change in the motion of an object"
frequency = dcc "frequency" (cn' "frequency")
  "the number of occurrences of a repeating event per unit of time"
friction = dcc "friction" (cn' "friction")
  "the force resisting the relative motion of two surfaces"
fOfGravity = dcc "fOfGravity" (cn "force of gravity")
  "the force exerted by gravity on an object"
gravity = dcc "gravity" (cn "gravity")
  "the force that attracts one physical body with mass to another"
gravitationalAccel = dcc "gravitationalAccel" (cn "gravitational acceleration")
  "the approximate acceleration due to gravity on Earth at sea level"
gravitationalConst = dcc "gravitationalConst" (cn "gravitational constant")
  "the empirical physical constant used to show the force between two objects caused by gravity"
height = dccWDS "height" (cn' "height")
  (S "the" +:+ phrase distance +:+ S "above a reference point for a point of interest")
isotropy = dccWDS "isotropy" (cn "isotropy")
  (S "a condition where the" +:+ phrase value `sOf` S "a" +:+ phrase property `sIs`
   S "independent of the direction in which it is measured")
joint = dcc "joint" (cn' "joint")
  "a connection between two rigid bodies which allows movement with one or more degrees of freedom"
kEnergy = dccWDS "kEnergy" (cn "kinetic energy")
  (S "measure" `ofThe` phrase energy +:+ S "a body possesses due to its motion")
kinematics = dccWDS "kinematics" (cn "kinematics")
  (S "branch" `sOf` phrase mechanics +:+ S "that describes the motion" `sOf` 
    S "objects without reference to the causes of motion")
linear = dcc "linear" (cn' "linear")
  "arranged in or extending along a straight or nearly straight line"
mechEnergy = dcc "mechEnergy" (cn "mechanical energy")
  "the energy that comes from motion and position"
momentum = dccWDS "momentum" (cn "momentum")
  ( S "the quantity of motion" `sOf` S "a moving body, measured as a product" `sOf` phrase QPP.mass `sAnd`
   phrase velocity)
moment = dccWDS "moment" (cn' "moment")
  (S "A measure of the tendency of a body to rotate about a specific" +:+ phrase point `sOr` phrase axis)
period = dccWDS "period" (cn' "period")
   (S "the" +:+ phrase time +:+ S "required for one complete cycle of vibration to pass a given point.")
pendulum = dccWDS "pendulum" (cn "pendulum")
 (S "a body suspended from a fixed support so that it swings freely back and forth under the influence" 
       `sOf` phrase gravity)
position = dcc "position" (cn' "position")
  "an object's location relative to a reference point"
positionVec = dccWDS " positionVec" (cn' "position vector")
   (S "a vector from the origin" `ofThe` phrase cartesian +:+ S "defined"
    `toThe` phrase point +:+ S "where the" +:+ phrase force +:+ S "is applied")
potEnergy = dccWDS "potEnergy" (cn "potential energy")
  (S "measure" `ofThe` phrase energy +:+ S "held by an object because of its" +:+ phrase position)
pressure = dccWDS "pressure" (cn' "pressure")
  (S "a" +:+ phrase force +:+ S "exerted over an area")
rectilinear = dcc "rectilinear" (cn "rectilinear")
  "occuring in one dimension"
rigidBody = dcc "rigidBody" (cnIES "rigid body")
  "a solid body in which deformation is neglected"
space = dcc "space" (cn' "space")
  "a two-dimensional extent where objects and events have relative positions and directions"
scalarAccel = dccWDS "scalarAccel" (cn' "scalar acceleration")
  (S "magnitude" `ofThe` phrase acceleration +:+ S "vector")
scalarPos = dccWDS "scalarPos" (cn' "scalar position")
  (S "magnitude" `ofThe` phrase position +:+ S "vector")
speed = dccWDS "speed" (cn' "speed")
  (S "magnitude" `ofThe` phrase velocity +:+ S "vector")
strain = dccWDS "strain" (cn' "strain") 
  (S "a measure of deformation representing the" +:+ phrase displacement +:+
   S "between particles in the body relative to a reference length")
  --definition of strain used in SSP, can be made clearer
stress = dcc "stress" (cn''' "stress")
  "the ratio of an applied force to a cross-sectional area"
  --definition of stress used in SSP, can be made clearer
tension = dccWDS "tension" (cn' "tension")
  (S "a" +:+ phrase stress +:+ S "that causes displacement of the body away from its center")
time = dcc "time" (cn' "time")
  "the indefinite continued progress of existence and events in the past, present, and future regarded as a whole"
torque = dcc "torque" (cn' "torque")
  "a twisting force that tends to cause rotation"
velocity = dccWDS "velocity" (cnIES "velocity")
  (S "the rate of change of a body's" +:+ phrase position)
weight = dcc "weight" (cn' "weight")
  "the gravitational force acting on an object"


-- Some variants of distance, speed, velocity, and scalar acceleration
-- FIXME: Complete all variants?
-- FIXME: Pull out commonalities?

xDist = dccWDS "xDist" (nounPhraseSent $ phrase distance +:+ S "in the" +:+ phrase xDir) (atStart distance +:+ S "in the" +:+ phrase xDir)
yDist = dccWDS "yDist" (nounPhraseSent $ phrase distance +:+ S "in the" +:+ phrase yDir) (atStart distance +:+ S "in the" +:+ phrase yDir)

iPos = dccWDS "iPos" (cn "initial position") (S "The" +:+ phrase position +:+ S "at the body's initial point")
xPos = dccWDS "xPos" (nounPhraseSent $ phrase xComp `sOf` phrase position) (S "The" +:+ phrase xComp `sOf` phrase position)
yPos = dccWDS "yPos" (nounPhraseSent $ phrase yComp `sOf` phrase position) (S "The" +:+ phrase yComp `sOf` phrase position)

ixPos = dccWDS "ixPos" (nounPhraseSent $ phrase xComp `sOf` phrase iPos) (S "The" +:+ phrase xComp `sOf` phrase iPos)
iyPos = dccWDS "iyPos" (nounPhraseSent $ phrase yComp `sOf` phrase iPos) (S "The" +:+ phrase yComp `sOf` phrase iPos)

fSpeed = dccWDS "fSpeed" (cn "final speed")   (S "The" +:+ phrase speed +:+ S "at the body's final point")
iSpeed = dccWDS "iSpeed" (cn "initial speed") (S "The" +:+ phrase speed +:+ S "at the body's initial point")

fVel = dccWDS "fVel" (cn "final velocity")   (S "The" +:+ phrase velocity +:+ S "at the body's final point")
iVel = dccWDS "iVel" (cn "initial velocity") (S "The" +:+ phrase velocity +:+ S "at the body's initial point")
xVel = dccWDS "xVel" (nounPhraseSent $ phrase xComp `sOf` phrase velocity) (S "The" +:+ phrase xComp `sOf` phrase velocity)
yVel = dccWDS "yVel" (nounPhraseSent $ phrase yComp `sOf` phrase velocity) (S "The" +:+ phrase yComp `sOf` phrase velocity)

ixVel = dccWDS "ixVel" (nounPhraseSent $ phrase xComp `sOf` phrase iVel) (S "The" +:+ phrase xComp `sOf` phrase iVel)
iyVel = dccWDS "iyVel" (nounPhraseSent $ phrase yComp `sOf` phrase iVel) (S "The" +:+ phrase yComp `sOf` phrase iVel)

xAccel = dccWDS "xScalAcc" (nounPhraseSent $ phrase xComp `sOf` phrase acceleration) (S "The" +:+ phrase xComp `sOf` phrase acceleration)
yAccel = dccWDS "yScalAcc" (nounPhraseSent $ phrase yComp `sOf` phrase acceleration) (S "The" +:+ phrase yComp `sOf` phrase acceleration)

constAccelV = dccWDS "constAccelV" (cn "constant acceleration vector") (S "The" +:+ phrase constAccel +:+ S "vector")
xConstAccel = dccWDS "xConstAccel" (nounPhraseSent $ phrase xComp `sOf` phrase constAccel) (S "The" +:+ phrase xComp `sOf` phrase constAccel)
yConstAccel = dccWDS "yConstAccel" (nounPhraseSent $ phrase yComp `sOf` phrase constAccel) (S "The" +:+ phrase yComp `sOf` phrase constAccel)


--FIXME: COMBINATION HACK (for all below)
angDisp = dcc "angularDisplacement" (compoundPhrase' (angular ^. term) (displacement ^. term))
  "the angle through which an object moves on a circular path"
angVelo = dcc "angularVelocity" (compoundPhrase' (angular ^. term) (velocity ^. term))
  "the rate of change of angular position of a rotating body"
angAccel = dcc "angularAcceleration" (compoundPhrase' (angular ^. term) (acceleration ^. term))
  "the rate of change of angular velocity"
constAccel = dcc "constantAcceleration" (cn "constant acceleration")
  "a one-dimensional acceleration that is constant"
linDisp = dcc "linearDisplacement" (compoundPhrase' (linear ^. term) (displacement ^. term)) 
  "movement in one direction along a single axis"
linVelo = dcc "linearVelocity" (compoundPhrase' (linear ^. term) (velocity ^. term)) 
  "the speed of a moving object, dependent on the perspective taken"
linAccel = dcc "linearAcceleration" (compoundPhrase' (linear ^. term) (acceleration ^. term)) 
  "the rate of change of velocity without a change in direction"

-- The following feel like they're missing something/need to be more
-- descriptive. See issue tracker for details.  
-- FIXME: plurals below?
restitutionCoef = dcc "restitutionCoef" (cn "coefficient of restitution")
  "a measure of the restitution of a collision between two objects"
momentOfInertia = dcc "momentOfInertia" (cn "moment of inertia")
  "a quantity expressing a body's tendency to resist angular acceleration"
angFreq = dcc "angularFrequency" (cn "angular frequency")
  "the frequency of a periodic process, wave system etc, per unit time."
--FIXME: These two should be built off "impulse"
impulseV = dcc "impulseV" (cn "impulse (vector)")
  "a force acting briefly on a body and producing a finite change of momentum in a given direction" 
impulseS = dcc "impulseS" (cn "impulse (scalar)")
  "a force acting briefly on a body and producing a finite change of momentum" 
