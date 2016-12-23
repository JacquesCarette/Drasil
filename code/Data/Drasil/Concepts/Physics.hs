module Data.Drasil.Concepts.Physics where
--This is obviously a bad name, but for now it will do until we come
--  up with a better one.
import Language.Drasil

rigidBody, velocity, angularV, friction, elasticity, collision, space,
  cartesian, rightHand, surface, restitutionCoef, acceleration,
  angularAccel, momentOfInertia, force, impulseS, impulseV, displacement,
  gravitationalAccel, gravitationalConst, position, distance, angularDisplacement,time, torque :: ConceptChunk
    
rigidBody  = dcc "rigidBody" "rigid body" 
  "A solid body in which deformation is neglected."
velocity   = dcc "velocity" "velocity" "The rate of change of a body's position."
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
cartesian  = dcc "cartesian" "Cartesian coordinates" ("A coordinate system that " ++
    "specifies each point uniquely in a plane by a pair of numerical " ++
    "coordinates.")
rightHand  = dcc "rightHand" "right-handed coordinate system" 
  "A coordinate system where the positive z-axis comes out of the screen."
  
-- The following feel like they're missing something/need to be more
-- descriptive. See issue tracker for details.  
surface    = dcc "surface" "surface" "surface" 
restitutionCoef = dcc "restitutionCoef" "coefficient of restitution" 
  "coefficient of restitution"
acceleration = dcc "acceleration" "acceleration" "acceleration"
angularAccel = dcc "angularAccel" "angular acceleration" "angular acceleration"
momentOfInertia = dcc "momentOfInertia" "moment of inertia" "moment of inertia"
force      = dcc "force" "force" "force"
impulseV   = dcc "impulseV" "impulse (vector)" "impulse (vector)"
impulseS   = dcc "impulseS" "impulse (scalar)" "impulse (scalar)"
position   = dcc "position" "position" "position"
distance   = dcc "distance" "distance" "distance"
displacement = dcc "displacement" "displacement" "displacement"
angularDisplacement = dcc "angularDisplacement" "angular displacement"
  "angular displacement"
time = dcc "time" "time" "time"
torque = dcc "torque" "torque" "torque"


gravitationalAccel = dcc "gravitationalAccel" "gravitational acceleration" 
  "gravitational acceleration" 
gravitationalConst = dcc "gravitationalConst" "gravitational constant" 
  "gravitational constant (6.673 * 10E-11)"