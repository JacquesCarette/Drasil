module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Data.Drasil.Concepts.Physics as CP
import Data.Drasil.SI_Units


acceleration, angularAccel, restitutionCoef, force, momentOfInertia,
  impulseS, impulseV, gravitationalAccel, gravitationalConst, displacement,
  angularVelocity, position, distance, angularDisplacement, torque, 
  linearDisplacement, velocity, linearVelocity, linearAccel :: ConVar


restitutionCoef = cvR CP.restitutionCoef (sub cC cR)

force = cvR CP.force (vec cF)
momentOfInertia = cvR CP.momentOfInertia (vec cI)
impulseV = cvR CP.impulseV (vec cJ)
impulseS = cvR CP.impulseS lJ
position = cvR CP.position (vec lP)
distance = cvR CP.distance lR
displacement = cvR CP.displacement (vec lR)
velocity = cvR CP.velocity (vec lV)
acceleration = cvR CP.acceleration (vec lA)
angularDisplacement = cvR CP.angDisp (Greek Theta_L)
angularVelocity = cvR CP.angVelo (Greek Omega_L)
angularAccel = cvR CP.angAccel (Greek Alpha_L)
linearDisplacement = cvR CP.linDisp (Concat [(vec lR), Atomic "(",lT, Atomic ")"])
linearVelocity = cvR CP.linVelo (Concat [(vec lV), Atomic "(", lT, Atomic ")"])
linearAccel = cvR CP.linAccel (Concat [(vec lA), Atomic "(", lT, Atomic ")"])
torque = cvR CP.torque (Greek Tau_L)

gravitationalAccel = cvR CP.gravitationalAccel lG
gravitationalConst = cvR CP.gravitationalConst cG



--FIXME: COnvert the ConVar values with units into UnitalChunks
time, energy :: UnitalChunk
--force         = uc CP.force (vec cF) newton
time          = uc CP.time lT second
energy        = uc CP.energy cE joule
