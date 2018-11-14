module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Data.Drasil.Concepts.Physics as CP (angAccel, angDisp, angVelo, 
    acceleration, displacement, distance, energy, force, gravitationalAccel, 
    gravitationalConst, impulseS, impulseV, linAccel, linDisp, linVelo, 
    momentOfInertia, position, pressure, restitutionCoef, time, torque, velocity)
import Data.Drasil.SI_Units (joule, metre, newton, pascal, radian, second)
import Data.Drasil.Units.Physics (accelU, angAccelU, angVelU, gravConstU, 
    impulseU, momtInertU, torqueU, velU)

restitutionCoef :: DefinedQuantityDict
restitutionCoef = dqd' CP.restitutionCoef (const $ sub cC cR) Real Nothing

physicscon :: [UnitalChunk]
physicscon = [angularAccel, angularDisplacement, angularVelocity, accelerationUC, displacementUC,
  distanceUC, energyUC, forceUC, gravitationalAccelUC, gravitationalConstUC, impulseSUC,
  impulseVUC, linearAccel, linearDisplacement, linearVelocity, momentOfInertiaUC,
  positionUC, pressureUC, timeUC, torqueUC, velocityUC]

angularAccel, angularDisplacement, angularVelocity, accelerationUC, displacementUC,
  distanceUC, energyUC, forceUC, gravitationalAccelUC, gravitationalConstUC, impulseSUC,
  impulseVUC, linearAccel, linearDisplacement, linearVelocity, momentOfInertiaUC,
  positionUC, pressureUC, timeUC, torqueUC, velocityUC :: UnitalChunk

angularAccel         = uc CP.angAccel lAlpha angAccelU
angularDisplacement  = uc CP.angDisp lTheta radian
angularVelocity      = uc CP.angVelo lOmega angVelU
accelerationUC       = uc CP.acceleration (vec lA) accelU
displacementUC       = uc CP.displacement (vec lR) metre
distanceUC           = uc CP.distance lR metre
energyUC             = uc CP.energy cE joule
forceUC              = uc CP.force (vec cF) newton
gravitationalAccelUC = uc CP.gravitationalAccel lG accelU
gravitationalConstUC = uc CP.gravitationalConst cG gravConstU
impulseSUC           = uc CP.impulseS lJ impulseU
impulseVUC           = uc CP.impulseV (vec cJ) impulseU
linearAccel          = uc CP.linAccel (Concat [(vec lA), Atomic "(", lT, Atomic ")"]) accelU
linearDisplacement   = uc CP.linDisp (Concat [(vec lR), Atomic "(",lT, Atomic ")"]) metre
linearVelocity       = uc CP.linVelo (Concat [(vec lV), Atomic "(", lT, Atomic ")"]) velU
momentOfInertiaUC    = uc CP.momentOfInertia (vec cI) momtInertU
positionUC           = uc CP.position (vec lP) metre
pressureUC           = uc CP.pressure lP pascal
timeUC               = uc CP.time lT second
torqueUC             = uc CP.torque lTau torqueU
velocityUC           = uc CP.velocity (vec lV) velU
