module Data.Drasil.Concepts.Physics where
--This is obviously a bad name, but for now it will do until we come
--  up with a better one.
import Language.Drasil

rigidBody, velocity, angularV, friction, elasticity, collision, space,
  cartesian, rightHand, surface, restitutionCoef, acceleration,
  angularAccel, momentOfInertia, force, impulseS, impulseV, displacement,
  gravitationalAccel, gravitationalConst, position, distance, angularDisplacement,time, torque :: NamedChunk
    
rigidBody  = makeCC "rigid body" 
  "A solid body in which deformation is neglected."
velocity   = makeCC "velocity" "The rate of change of a body's position."
angularV   = makeCC "angular velocity" 
  "The rate of change of a body's orientation."
friction   = makeCC "friction" 
  "The force resisting the relative motion of two surfaces."
elasticity = makeCC "elasticity" ("Ratio of the relative velocities " ++
    "of two colliding objects after and before a collision.")
collision  = makeCC "collision" ("An encounter between particles resulting " ++
    "in an exchange or transformation of energy.")
space      = makeCC "space" ("A two-dimensional extent where objects and " ++
    "events have relative positions and directions.")
cartesian  = makeCC "Cartesian coordinates" ("A coordinate system that " ++
    "specifies each point uniquely in a plane by a pair of numerical " ++
    "coordinates.")
rightHand  = makeCC "right-handed coordinate system" 
  "A coordinate system where the positive z-axis comes out of the screen."
  
-- The following feel like they're missing something/need to be more
-- descriptive. See issue tracker for details.  
surface    = nCC "surface" 
restitutionCoef = nCC "coefficient of restitution"
acceleration    = nCC "acceleration"
angularAccel    = nCC "angular acceleration"
momentOfInertia = nCC "moment of inertia"
force      = nCC "force"
impulseV   = nCC "impulse (vector)"
impulseS   = nCC "impulse (scalar)"
position   = nCC "position"
distance   = nCC "distance"
displacement = nCC "displacement"
angularDisplacement = nCC "angular displacement"
time = nCC "time"
torque = nCC "torque"


gravitationalAccel = nCC "gravitational acceleration" 
gravitationalConst = makeCC "gravitational constant" 
  "gravitational constant (6.673 * 10E-11)"