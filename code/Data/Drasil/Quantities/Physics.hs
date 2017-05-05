module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Data.Drasil.Concepts.Physics as CP

surface, acceleration, angularAccel, restitutionCoef, force, momentOfInertia,
  impulseS, impulseV, gravitationalAccel, gravitationalConst, displacement,
  angularV, position, distance, angularDisplacement,time, torque, 
  linearDisplacement, velocity, linearVelocity, linearAccel :: ConVar

surface = cvR CP.surface cS --Maybe should be physical property?
restitutionCoef = cvR CP.restitutionCoef (sub cC cR)
acceleration = cvR CP.acceleration (vec lA)
angularAccel = cvR CP.angularAccel (Greek Alpha_L)
angularV = cvR CP.angularV (Greek Omega_L)
force = cvR CP.force (vec cF)
momentOfInertia = cvR CP.momentOfInertia (vec cI)
impulseV = cvR CP.impulseV (vec cJ)
impulseS = cvR CP.impulseS lJ
position = cvR CP.position (vec lP)
distance = cvR CP.distance lR
displacement = cvR CP.displacement (vec lR)
linearDisplacement = cvR CP.linearDisplacement (Concat [(vec lR), Atomic "(",lT, Atomic ")"])
linearVelocity = cvR CP.linearVelocity (Concat [(vec lV), Atomic "(", lT, Atomic ")"])
linearAccel = cvR CP.linearAccel (Concat [(vec lA), Atomic "(", lT, Atomic ")"])
time = cvR CP.time lT
torque = cvR CP.torque (Greek Tau_L)
angularDisplacement = cvR CP.angularDisplacement (Greek Theta_L)
velocity = cvR CP.velocity (vec lV)

gravitationalAccel = cvR CP.gravitationalAccel lG
gravitationalConst = cvR CP.gravitationalConst cG
