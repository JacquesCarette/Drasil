module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Data.Drasil.Concepts.Physics as CP
import Data.Drasil.Units.Physics
import Data.Drasil.SI_Units

restitutionCoef :: DefinedQuantityDict
restitutionCoef = cqs CP.restitutionCoef (sub cC cR) Real

angularAccel, angularDisplacement, angularVelocity, acceleration, displacement,
  distance, energy, force, gravitationalAccel, gravitationalConst, impulseS,
  impulseV, linearAccel, linearDisplacement, linearVelocity, momentOfInertia,
  position, pressure, time, torque, velocity :: UnitalChunk

angularAccel        = ucEL CP.angAccel (Greek Alpha_L) angAccelU
angularDisplacement = ucEL CP.angDisp (Greek Theta_L) radian
angularVelocity     = ucEL CP.angVelo (Greek Omega_L) angVelU
acceleration        = ucEL CP.acceleration (vec lA) accelU
displacement        = ucEL CP.displacement (vec lR) metre
distance            = ucEL CP.distance lR metre
energy              = ucEL CP.energy cE joule
force               = ucEL CP.force (vec cF) newton
gravitationalAccel  = ucEL CP.gravitationalAccel lG accelU
gravitationalConst  = ucEL CP.gravitationalConst cG gravConstU
impulseS            = ucEL CP.impulseS lJ impulseU
impulseV            = ucEL CP.impulseV (vec cJ) impulseU
linearAccel         = ucEL CP.linAccel (Concat [(vec lA), Atomic "(", lT, Atomic ")"]) accelU
linearDisplacement  = ucEL CP.linDisp (Concat [(vec lR), Atomic "(",lT, Atomic ")"]) metre
linearVelocity      = ucEL CP.linVelo (Concat [(vec lV), Atomic "(", lT, Atomic ")"]) velU
momentOfInertia     = ucEL CP.momentOfInertia (vec cI) momtInertU
position            = ucEL CP.position (vec lP) metre
pressure            = ucEL CP.pressure lP pascal
time                = ucEL CP.time lT second
torque              = ucEL CP.torque (Greek Tau_L) torqueU
velocity            = ucEL CP.velocity (vec lV) velU
