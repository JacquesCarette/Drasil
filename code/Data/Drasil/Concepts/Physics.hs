module Data.Drasil.Concepts.Physics where
--This is obviously a bad name, but for now it will do until we come
--  up with a better one.
import Language.Drasil
import Control.Lens ((^.))

rigidBody, velocity, angularV, friction, elasticity, collision, space,
  cartesian, rightHand, surface, restitutionCoef, acceleration,
  angularAccel, momentOfInertia, force, impulseS, impulseV, displacement,
  gravitationalAccel, gravitationalConst, position, distance, angularDisplacement,time, torque :: ConceptChunk
    
rigidBody  = dcc "rigidBody" "rigid body" 
  "A solid body in which deformation is neglected."
velocity   = dccWDS "velocity" "velocity" 
  (S "The rate of change of a body's " :+: (position ^. term))
angularV   = dcc "angularV" "angular velocity" 
  "The rate of change of a body's orientation."
friction   = dcc "friction" "friction" 
  "The force resisting the relative motion of two surfaces."
elasticity = dcc "elasticity" "elasticity" ("Ratio of the relative velocities " ++
    "of two colliding objects after and before a collision.")
collision  = dcc "collision" "collision" ("An encounter between particles resulting " ++
    "in an exchange or transformation of energy.")
space      = dcc "space" "space" ("A two-dimensional extent where objects and " ++
    "events have relative positions and directions.")
cartesian  = dcc "cartesian" "Cartesian coordinate" ("A coordinate system that " ++
    "specifies each point uniquely in a plane by a pair of numerical " ++
    "coordinates.")
rightHand  = dcc "rightHand" "right-handed coordinate system" 
  "A coordinate system where the positive z-axis comes out of the screen."
  
  
position   = dcc "position" "position" 
  "an object's location relative to a reference point"
acceleration = dccWDS "acceleration" "acceleration" 
  (S "the rate of change of a body's " :+: (velocity ^. term))
angularAccel = dccWDS "angularAccel" "angular acceleration" 
  (S "the rate of change of a body's " :+: (angularV ^. term))  
displacement = dccWDS "displacement" "displacement" 
  (S "the change in " :+: (position ^. defn))
force      = dcc "force" "force" 
  "an interaction that tends to produce change in the motion of an object"
surface    = dcc "surface" "surface"
  "the outer or topmost boundary of an object"

-- The following feel like they're missing something/need to be more
-- descriptive. See issue tracker for details.  
restitutionCoef = dcc "restitutionCoef" "coefficient of restitution" 
  "coefficient of restitution"
momentOfInertia = dcc "momentOfInertia" "moment of inertia" "moment of inertia"
impulseV   = dcc "impulseV" "impulse (vector)" "impulse (vector)"
impulseS   = dcc "impulseS" "impulse (scalar)" "impulse (scalar)"
distance   = dcc "distance" "distance" "distance"
angularDisplacement = dcc "angularDisplacement" "angular displacement"
  "angular displacement"
time = dcc "time" "time" "time"
torque = dcc "torque" "torque" "torque"


gravitationalAccel = dcc "gravitationalAccel" "gravitational acceleration" 
  "gravitational acceleration" 
gravitationalConst = dcc "gravitationalConst" "gravitational constant" 
  "gravitational constant (6.673 * 10E-11)"