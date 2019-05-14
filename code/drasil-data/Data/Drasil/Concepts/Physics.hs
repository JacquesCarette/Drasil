module Data.Drasil.Concepts.Physics 
  ( rigidBody, velocity, friction, elasticity, energy, mechEnergy, collision, space
  , cartesian, rightHand, restitutionCoef, acceleration, pressure
  , momentOfInertia, force, impulseS, impulseV, displacement
  , gravitationalAccel, gravitationalConst, position, distance
  , time, torque, weight, fbd, angular, linear, tension, compression, stress
  , strain , angDisp, angVelo, angAccel, linDisp, linVelo, linAccel, joint
  , damping , cohesion, isotropy, twoD, threeD, physicCon, physicCon', kEnergy
  ) where
--This is obviously a bad name, but for now it will do until we come
--  up with a better one.
import Language.Drasil
import Data.Drasil.IdeaDicts (physics)
import Data.Drasil.Concepts.Documentation (property, value)
import Data.Drasil.SentenceStructures (sOf)
import Control.Lens((^.)) --need for parametrization hack

physicCon :: [ConceptChunk]
physicCon = [rigidBody, velocity, friction, elasticity, energy, mechEnergy, collision, space,
  cartesian, rightHand, restitutionCoef, acceleration,
  momentOfInertia, force, impulseS, impulseV, displacement,
  gravitationalAccel, gravitationalConst, position, distance,
  time, torque, weight, fbd, linear, angular, tension, compression, stress, 
  strain, angDisp, angVelo, angAccel, linDisp, linVelo, linAccel, 
  joint, damping, pressure, cohesion, isotropy, kEnergy]


physicCon' :: [CI]
physicCon' = [twoD, threeD]

rigidBody, velocity, friction, elasticity, energy, mechEnergy, collision, space,
  cartesian, rightHand, restitutionCoef, acceleration,
  momentOfInertia, force, impulseS, impulseV, displacement,
  gravitationalAccel, gravitationalConst, position, distance,
  time, torque, weight, fbd, linear, angular, tension, compression, stress, 
  strain, angDisp, angVelo, angAccel, linDisp, linVelo, linAccel, 
  joint, damping, pressure,cohesion, isotropy, kEnergy :: ConceptChunk

  -- joint, damping, pressure, cohesion, isotropy :: ConceptChunk

twoD, threeD :: CI

rigidBody    = dcc "rigidBody" (cnIES "rigid body") 
  "A solid body in which deformation is neglected."
velocity     = dccWDS "velocity" (cnIES "velocity")
  (S "The rate of change of a body's" +:+ (phrase position))
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
  (S "The rate of change of a body's" +:+ (phrase velocity))
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

twoD = commonIdeaWithDict "twoD" (pn "two-dimensional") "2D" [physics]

threeD = commonIdeaWithDict "threeD" (pn "three-dimensional") "3D" [physics]