module Data.Drasil.Concepts.Physics where
--This is obviously a bad name, but for now it will do until we come
--  up with a better one.
import Language.Drasil
import Utils.Drasil

import Data.Drasil.IdeaDicts (physics)
import Data.Drasil.Concepts.Documentation (property, value)
import Control.Lens((^.)) --need for parametrization hack

physicCon :: [ConceptChunk]
physicCon = [acceleration, angAccel, angDisp, angVelo, angular, cartesian,
  cohesion, collision, compression, damping, displacement, distance, elasticity,
  energy, fbd, force, friction, fSpeed, fVel, gravitationalAccel,
  gravitationalConst, impulseS, impulseV, isotropy, iSpeed, iVel, ixVel, joint,
  kEnergy, linAccel, linDisp, linVelo, linear, mechEnergy, momentOfInertia,
  position, pressure, restitutionCoef, rightHand, rigidBody, scalarAccel, space,
  speed, strain, stress, tension, time, torque, velocity, weight, xDist, xVel,
  yDist, yVel, xAccel, yAccel]

physicCon' :: [CI]
physicCon' = [twoD, threeD]

acceleration, angAccel, angDisp, angVelo, angular, cartesian, cohesion,
  collision, compression, damping, displacement, distance, elasticity, energy,
  fbd, force, friction, gravitationalAccel, fSpeed, fVel, gravitationalConst,
  impulseS, impulseV, isotropy, iSpeed, iVel, ixVel, joint, kEnergy, linAccel,
  linDisp, linVelo, linear, mechEnergy, momentOfInertia, position, pressure,
  restitutionCoef, rightHand, rigidBody, scalarAccel, space, speed, strain,
  stress, tension, time, torque, velocity, weight, xDist, xVel, yDist,
  yVel, xAccel, yAccel :: ConceptChunk

  -- joint, damping, pressure, cohesion, isotropy :: ConceptChunk

twoD, threeD :: CI
twoD   = commonIdeaWithDict "twoD"   (pn "two-dimensional")   "2D" [physics]
threeD = commonIdeaWithDict "threeD" (pn "three-dimensional") "3D" [physics]

rigidBody    = dcc "rigidBody" (cnIES "rigid body") 
  "A solid body in which deformation is neglected."
velocity     = dccWDS "velocity" (cnIES "velocity")
  (S "The rate of change of a body's" +:+ (phrase position))
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
cartesian    = dcc "cartesian" (pn' "Cartesian coordinate system") 
  ("A coordinate system that specifies each point uniquely in a plane by a " ++
  "pair of numerical coordinates.")
rightHand    = dcc "rightHand" (cn' "right-handed coordinate system")
  "A coordinate system where the positive z-axis comes out of the screen."
  
joint        = dcc "joint"    (cn' "joint") ("a connection between two rigid " ++ 
  "bodies which allows movement with one or more degrees of freedom")
kEnergy  = dccWDS "kEnergy" (cn "kinetic energy")
  (S "The measure of the" +:+ (phrase energy) +:+ 
   S "a body possess due to its motion.")
position     = dcc "position" (cn' "position")
  "An object's location relative to a reference point"
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
  (at_start' force +:+ S "that are exerted between planes internal to" +:+
  S "a larger body subject to external loading.")            --definition used in SSP, can be made clearer
strain       = dccWDS "strain" (cn' "strain")
  (S "A measure of deformation of a body or plane under" +:+. phrase stress) --definition used in SSP, can be made clearer
tension      = dccWDS "tension" (cn' "tension")
  (S "A" +:+ (phrase stress) +:+
  S "that causes displacement of the body away from its center.")
compression  = dccWDS "compression" (cn' "compression")
  (S "A" +:+ (phrase stress) +:+
  S "that causes displacement of the body towards its center.")

pressure     = dccWDS "pressure" (cn' "pressure")
  (S "A" +:+ (phrase force) +:+ S "exerted over an area")

-- Some variants of distance, speed, velocity, and scalar acceleration
-- FIXME: Complete all variants?
-- FIXME: Pull out commonalities?

xDist = dccWDS "xDist" (cn "distance in the x-direction") (at_start distance +:+ S "in the x-direction")
yDist = dccWDS "yDist" (cn "distance in the y-direction") (at_start distance +:+ S "in the y-direction")

fSpeed = dccWDS "fSpeed" (cn "final speed")   (S "The" +:+ phrase speed +:+ S "at the body's final point")
iSpeed = dccWDS "iSpeed" (cn "initial speed") (S "The" +:+ phrase speed +:+ S "at the body's initial point")

fVel = dccWDS "fVel" (cn "final velocity")   (S "The" +:+ phrase velocity +:+ S "at the body's final point")
iVel = dccWDS "iVel" (cn "initial velocity") (S "The" +:+ phrase velocity +:+ S "at the body's initial point")
xVel = dccWDS "xVel" (cn "x-component of velocity") (S "The x-component of" +:+ phrase velocity)
yVel = dccWDS "yVel" (cn "y-component of velocity") (S "The y-component of" +:+ phrase velocity)

ixVel = dccWDS "ixVel" (cn "x-component of initial velocity") (S "The x-component of initial" +:+ phrase velocity)

xAccel = dccWDS "xScalAcc" (cn "x-component of acceleration") (S "The x-component of" +:+ phrase acceleration)
yAccel = dccWDS "yScalAcc" (cn "y-component of acceleration") (S "The y-component of" +:+ phrase acceleration)

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

gravitationalAccel = dcc "gravitationalAccel" 
  (cn "gravitational acceleration") "An expression used in physics to indicate the intensity of a gravitational field"
gravitationalConst = dcc "gravitationalConst" (cn "gravitational constant" )
  "gravitational constant (6.673 * 10E-11)"

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
  phrase force +:+ S "between adjacent particles that holds the matter" +:+
  S "together.")

isotropy = dccWDS "isotropy" (cn "isotropy") (S "A condition where the" +:+
  phrase value `sOf` S "a" +:+ phrase property +:+ S "is independent of" +:+
  S "the direction in which it is measured.")
