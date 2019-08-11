module Data.Drasil.Concepts.Physics where
--This is obviously a bad name, but for now it will do until we come
--  up with a better one.
import Language.Drasil
import Utils.Drasil

import Data.Drasil.IdeaDicts (mathematics, physics)
import Data.Drasil.Concepts.Documentation (property, value)
import Data.Drasil.Concepts.Math (xComp, xDir, yComp, yDir)
import Control.Lens((^.)) --need for parametrization hack

physicCon :: [ConceptChunk]
physicCon = [acceleration, angAccel, angDisp, angVelo, angular, chgInVelocity,
  cohesion, collision, compression, constAccel, constAccelV, damping,
  displacement, distance, elasticity, energy, fSpeed, fVel, fbd, force,
  friction, gravity, gravitationalAccel, gravitationalConst, height, iPos,
  iSpeed, iVel, impulseS, impulseV, isotropy, ixPos, ixVel, iyPos, iyVel,
  joint, kEnergy, linAccel, linDisp, linVelo, linear, mechEnergy,
  momentOfInertia, position, potEnergy, pressure, restitutionCoef, rectilinear,
  rigidBody, scalarAccel, scalarPos, space, speed, strain, stress, tension,
  time, torque, velocity, weight, xAccel, xConstAccel, xDist, xPos, xVel,
  yAccel, yConstAccel, yDist, yPos, yVel]

physicCon' :: [CI]
physicCon' = [oneD, twoD, threeD]

acceleration, angAccel, angDisp, angVelo, angular, chgInVelocity, cohesion,
  collision, compression, constAccel, constAccelV, damping, displacement,
  distance, elasticity, energy, fSpeed, fVel, fbd, force, friction, gravity,
  gravitationalAccel, gravitationalConst, height, iPos, iSpeed, iVel, impulseS,
  impulseV, isotropy, ixPos, ixVel, iyPos, iyVel, joint, kEnergy, linAccel,
  linDisp, linVelo, linear, mechEnergy, momentOfInertia, position, potEnergy,
  pressure, rectilinear, restitutionCoef, rigidBody, scalarAccel, scalarPos,
  space, speed, strain, stress, tension, time, torque, velocity, weight,
  xAccel, xConstAccel, xDist, xPos, xVel, yAccel, yConstAccel, yDist,
  yPos, yVel :: ConceptChunk

oneD, twoD, threeD :: CI
oneD   = commonIdeaWithDict "oneD"   (cn "one-dimensional")   "1D" [mathematics, physics]
twoD   = commonIdeaWithDict "twoD"   (cn "two-dimensional")   "2D" [mathematics, physics]
threeD = commonIdeaWithDict "threeD" (cn "three-dimensional") "3D" [mathematics, physics]

rigidBody    = dcc "rigidBody" (cnIES "rigid body") 
  "A solid body in which deformation is neglected."
velocity     = dccWDS "velocity" (cnIES "velocity")
  (S "The rate of change of a body's" +:+ phrase position)
speed        = dccWDS "speed" (cn' "speed")
  (S "The magnitude of the" +:+ phrase velocity +:+ S "vector")
friction     = dcc "friction" (cn' "friction")
  "The force resisting the relative motion of two surfaces."
elasticity   = dcc "elasticity" (cnIES "elasticity") 
  ("Ratio of the relative velocities " ++
  "of two colliding objects after and before a collision.")
energy       = dcc "energy" (cn "energy")
  "Power derived from the utilization of physical or chemical resources."
mechEnergy  = dcc "mechEnergy" (cn "mechanical energy")
  "The energy that comes from motion and position"
collision    = dcc "collision" (cn' "collision")
  ("An encounter between particles resulting " ++
  "in an exchange or transformation of energy.")
space        = dcc "space" (cn' "space") 
  ("A two-dimensional extent where objects and " ++
  "events have relative positions and directions.")
rectilinear  = dcc "rectilinear" (cn "rectilinear")
  "Occuring in one dimension."
  
joint        = dcc "joint"    (cn' "joint") ("a connection between two rigid " ++ 
  "bodies which allows movement with one or more degrees of freedom")
kEnergy  = dccWDS "kEnergy" (cn "kinetic energy")
  (S "The measure of the" +:+ phrase energy +:+ 
   S "a body possess due to its motion.")
position     = dcc "position" (cn' "position")
  "An object's location relative to a reference point"
scalarPos  = dccWDS "scalarPos" (cn' "scalar position")
  (S "The magnitude of the " +:+ phrase position +:+ S "vector")
acceleration = dccWDS "acceleration" (cn' "acceleration")
  (S "The rate of change of a body's" +:+ phrase velocity)
scalarAccel  = dccWDS "scalarAccel" (cn' "scalar acceleration")
  (S "The magnitude of the " +:+ phrase acceleration +:+ S "vector")
displacement = dccWDS "displacement" (cn' "displacement")
  (S "The change in" +:+ (position ^. defn))
force        = dcc "force" (cn' "force")
  "An interaction that tends to produce change in the motion of an object"
distance     = dcc "distance" (cn' "distance")
  "The interval measured along a path connecting two locations"
stress       = dccWDS "stress" (cn''' "stress")
  (atStart' force +:+ S "that are exerted between planes internal to" +:+
  S "a larger body subject to external loading.")            --definition used in SSP, can be made clearer
strain       = dccWDS "strain" (cn' "strain")
  (S "A measure of deformation of a body or plane under" +:+. phrase stress) --definition used in SSP, can be made clearer
tension      = dccWDS "tension" (cn' "tension")
  (S "A" +:+ phrase stress +:+
  S "that causes displacement of the body away from its center.")
compression  = dccWDS "compression" (cn' "compression")
  (S "A" +:+ phrase stress +:+
  S "that causes displacement of the body towards its center.")
pressure     = dccWDS "pressure" (cn' "pressure")
  (S "A" +:+ phrase force +:+ S "exerted over an area")
height       = dccWDS "height" (cn' "height") (S "The" +:+ phrase distance +:+ 
  S "above a reference point for a point of interest.")

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

potEnergy  = dccWDS "potEnergy" (cn "potential energy")
  (S "The measure of the" +:+ phrase energy +:+ 
   S "held by an object because of its" +:+  phrase position)

--FIXME: COMBINATION HACK (for all below)
angDisp = dcc "angularDisplacement" 
  (compoundPhrase' (angular ^. term) (displacement ^. term))
  "The angle through which an object moves on a circular path"
angVelo = dcc "angularVelocity" 
  (compoundPhrase' (angular ^. term) (velocity ^. term))
  "The rate of change of angular position of a rotating body"
angAccel = dcc "angularAcceleration"
  (compoundPhrase' (angular ^. term) (acceleration ^. term))
  "The rate of change of angular velocity"
constAccel = dcc "constantAcceleration"
  (cn "constant acceleration") "A one-dimensional acceleration that is constant"
linDisp = dcc "linearDisplacement" 
  (compoundPhrase' (linear ^. term) (displacement ^. term)) 
  "Movement in one direction along a single axis"
linVelo = dcc "linearVelocity" 
  (compoundPhrase' (linear ^. term) (velocity ^. term)) 
  "The speed of a moving object, dependent on the perspective taken"
linAccel = dcc "linearAcceleration" 
  (compoundPhrase' (linear ^. term) (acceleration ^. term)) 
  "The rate of change of velocity without a change in direction"

-- The following feel like they're missing something/need to be more
-- descriptive. See issue tracker for details.  
-- FIXME: plurals below?
restitutionCoef = dcc "restitutionCoef" (cn "coefficient of restitution")
   "A measure of the restitution of a collision between two objects"
momentOfInertia = dcc "momentOfInertia" (cn "moment of inertia")
   "A quantity expressing a body's tendency to resist angular acceleration"

--FIXME: These two should be built off "impulse"
impulseV   = dcc "impulseV" (cn "impulse (vector)")
   "A force acting briefly on a body and producing a finite change of momentum in a given direction" 
impulseS   = dcc "impulseS" (cn "impulse (scalar)") "A force acting briefly on a body and producing a finite change of momentum" 

gravity = dcc "gravity" (cn "gravity") "The force that attracts one physical body with mass to another."
gravitationalAccel = dcc "gravitationalAccel" 
  (cn "gravitational acceleration") "The approximate acceleration due to gravity on Earth at sea level"
gravitationalConst = dcc "gravitationalConst" (cn "gravitational constant" )
  "An empirical physical constant, used to show the force between two objects caused by gravity"

time   = dcc "time"   (cn' "time") 
  "The indefinite continued progress of existence and events in the past, present, and future regarded as a whole"
torque = dcc "torque" (cn' "torque") 
  "A twisting force that tends to cause rotation"

weight = dcc "weight" (cn' "weight") 
  "The gravitational force acting on an object"

fbd = dcc "FBD" (cn' "free body diagram")
  "A graphical illustration used to visualize the applied forces, movements, and resulting reactions on a body in a steady state condition"

linear  = dcc "linear"  (cn' "linear" )
  "Arranged in or extending along a straight or nearly straight line"
angular = dcc "angular" (cn' "angular")
  "Denoting physical properties or quantities measured with reference to or by means of an angle"

damping = dcc "damping" (cn' "damping")
  "An effect that tends to reduce the amplitude of vibrations"

cohesion = dccWDS "cohesion" (cn "cohesion") (S "An attractive" +:+ 
  phrase force +:+. S "between adjacent particles that holds the matter together")

isotropy = dccWDS "isotropy" (cn "isotropy") (S "A condition where the" +:+
  phrase value `sOf` S "a" +:+ phrase property +:+ S "is independent of" +:+
  S "the direction in which it is measured.")

chgInVelocity = dccWDS "chgInVelocity" (cn "change in velocity") (S "The" +:+ phrase chgInVelocity +:+
 S "of a" +:+ phrase rigidBody)

