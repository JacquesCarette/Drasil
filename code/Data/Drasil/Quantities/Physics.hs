module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Data.Drasil.Concepts.Physics as CP

surface, acceleration, angularAccel, restitutionCoef, force, momentOfInertia,
  impulseS, impulseV, gravitationalAccel, gravitationalConst, displacement
  angularV, position, distance, angularDisplacement,time, torque, 
  velocity :: VarChunk

surface = vcFromCC CP.surface cS --Maybe should be physical property?
restitutionCoef = vcFromCC CP.restitutionCoef (sub cC cR)
acceleration = vcFromCC CP.acceleration (vec lA)
angularAccel = vcFromCC CP.angularAccel (Greek Alpha_L)
angularV = vcFromCC CP.angularV (Greek Omega_L)
force = vcFromCC CP.force (vec cF)
momentOfInertia = vcFromCC CP.momentOfInertia (vec cI)
impulseV = vcFromCC CP.impulseV (vec cJ)
impulseS = vcFromCC CP.impulseS lJ
position = vcFromCC CP.position (vec lP)
distance = vcFromCC CP.distance lR
displacement = vcFromCC CP.displacement (vec lR)
time = vcFromCC CP.time lT
torque = vcFromCC CP.torque (Greek Tau_L)
angularDisplacement = vcFromCC CP.angularDisplacement (Greek Theta_L)
velocity = vcFromCC CP.velocity (vec lV)

gravitationalAccel = vcFromCC CP.gravitationalAccel lG
gravitationalConst = vcFromCC CP.gravitationalConst cG
