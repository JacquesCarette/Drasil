module Data.Drasil.Concepts.Physics 
  ( rigidBody, velocity, angularV, friction, elasticity, collision, space
  , cartesian, rightHand, surface, restitutionCoef, acceleration
  , angularAccel, momentOfInertia, force, impulseS, impulseV, displacement
  , gravitationalAccel, gravitationalConst, position, distance
  , angularDisplacement,time, torque
  ) where
--This is obviously a bad name, but for now it will do until we come
--  up with a better one.
import Language.Drasil
import Control.Lens ((^.))

rigidBody, velocity, angularV, friction, elasticity, collision, space,
  cartesian, rightHand, surface, restitutionCoef, acceleration,
  angularAccel, momentOfInertia, force, impulseS, impulseV, displacement,
  gravitationalAccel, gravitationalConst, position, distance, angularDisplacement,time, torque :: ConceptChunk

rigidBody  = dcc "rigidBody" 
  (cnIES "rigid body") "A solid body in which deformation is neglected."
velocity   = dccWDS "velocity" (cnIES "velocity")
  (S "The rate of change of a body's " :+: (phrase (position ^. term)))
angularV   = dcc "angularV" (cnIES "angular velocity")
  "The rate of change of a body's orientation."
friction   = dcc "friction" (cn' "friction")
  "The force resisting the relative motion of two surfaces."
elasticity = dcc "elasticity" (cnIES "elasticity") 
  ("Ratio of the relative velocities " ++
  "of two colliding objects after and before a collision.")
collision  = dcc "collision" (cn' "collision")
  ("An encounter between particles resulting " ++
  "in an exchange or transformation of energy.")
space      = dcc "space" (cn' "space") 
  ("A two-dimensional extent where objects and " ++
  "events have relative positions and directions.")
cartesian  = dcc "cartesian" (pn' "Cartesian coordinate") 
  ("A coordinate system that specifies each point uniquely in a plane by a " ++
  "pair of numerical coordinates.")
rightHand  = dcc "rightHand" (cn' "right-handed coordinate system")
  "A coordinate system where the positive z-axis comes out of the screen."
  
  
position   = dcc "position" (cn' "position")
  "an object's location relative to a reference point"
acceleration = dccWDS "acceleration" (cn' "acceleration")
  (S "the rate of change of a body's " :+: (phrase (velocity ^. term)))
angularAccel = dccWDS "angularAccel" (cn' "angular acceleration")
  (S "the rate of change of a body's " :+: (phrase (angularV ^. term)))
displacement = dccWDS "displacement" (cn' "displacement")
  (S "the change in " :+: (position ^. defn))
force      = dcc "force" (cn' "force")
  "an interaction that tends to produce change in the motion of an object"
surface    = dcc "surface" (cn' "surface")
  "the outer or topmost boundary of an object"
distance   = dcc "distance" (cn' "distance")
  "the interval measured along a path connecting two locations"

-- The following feel like they're missing something/need to be more
-- descriptive. See issue tracker for details.  
-- FIXME: plurals below?
--TODO: The 'FIXME's below.
fixme :: String
fixme = "FIXME: MISSING DEFINITION"

restitutionCoef = dcc "restitutionCoef" (cn "coefficient of restitution") fixme
momentOfInertia = dcc "momentOfInertia" (cn "moment of inertia") fixme

--FIXME: These two should be built off "impulse"
impulseV   = dcc "impulseV" (cn "impulse (vector)") fixme 
impulseS   = dcc "impulseS" (cn "impulse (scalar)") fixme 

angularDisplacement = dcc "angularDisplacement" (cn' "angular displacement") fixme
time = dcc "time" (cn' "time") fixme
torque = dcc "torque" (cn' "torque") fixme

gravitationalAccel = dcc "gravitationalAccel" 
  (cn "gravitational acceleration") fixme
gravitationalConst = dcc "gravitationalConst" (cn "gravitational constant" )
  "gravitational constant (6.673 * 10E-11)"
