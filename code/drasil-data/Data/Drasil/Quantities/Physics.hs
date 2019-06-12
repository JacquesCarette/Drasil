module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Language.Drasil.ShortHands
import qualified Data.Drasil.Concepts.Physics as CP (angAccel, angDisp, angVelo, 
    acceleration, height, displacement, distance, energy, force, gravitationalAccel, 
    gravitationalConst, impulseS, impulseV, linAccel, linDisp, linVelo, 
    momentOfInertia, position, pressure, restitutionCoef, time, torque, velocity,
    weight, kEnergy, chgInVelocity, potEnergy)

import Data.Drasil.SI_Units (joule, metre, newton, pascal, radian, second)
import Data.Drasil.Units.Physics (accelU, angAccelU, angVelU, gravConstU, 
    impulseU, momtInertU, torqueU, velU)

restitutionCoef :: DefinedQuantityDict
restitutionCoef = dqd' CP.restitutionCoef (const $ sub cC cR) Real Nothing

physicscon :: [UnitalChunk]
physicscon = [angularAccel, angularDisplacement, angularVelocity, acceleration,
  displacement, distance, energy, force, gravitationalAccel, gravitationalConst,
  height, impulseS, impulseV, linearAccel, linearDisplacement, linearVelocity,
  momentOfInertia, position, pressure, time, torque, velocity, weight, kEnergy,
  chgInVelocity]

angularAccel, angularDisplacement, angularVelocity, acceleration, displacement,
  distance, energy, force, gravitationalAccel, gravitationalConst, height, 
  impulseS, impulseV, linearAccel, linearDisplacement, linearVelocity,
  momentOfInertia, position, pressure, time, torque, velocity, weight,
  kEnergy, chgInVelocity, potEnergy :: UnitalChunk

angularAccel         = uc CP.angAccel lAlpha angAccelU
angularDisplacement  = uc CP.angDisp lTheta radian
angularVelocity      = uc CP.angVelo lOmega angVelU
acceleration         = uc CP.acceleration (vec lA) accelU
chgInVelocity        = uc CP.chgInVelocity (Concat [cDelta, vec lV]) velU
displacement         = uc CP.displacement (vec lR) metre
distance             = uc CP.distance lR metre
energy               = uc CP.energy cE joule
force                = uc CP.force (vec cF) newton
gravitationalAccel   = uc CP.gravitationalAccel lG accelU
gravitationalConst   = uc CP.gravitationalConst cG gravConstU
height               = uc CP.height lH metre
impulseS             = uc CP.impulseS lJ impulseU
impulseV             = uc CP.impulseV (vec cJ) impulseU
kEnergy              = uc CP.kEnergy  (Concat [cK, cE]) joule
linearAccel          = uc CP.linAccel (Concat [vec lA, Atomic "(", lT, Atomic ")"]) accelU
linearDisplacement   = uc CP.linDisp  (Concat [vec lR, Atomic "(", lT, Atomic ")"]) metre
linearVelocity       = uc CP.linVelo  (Concat [vec lV, Atomic "(", lT, Atomic ")"]) velU
momentOfInertia      = uc CP.momentOfInertia (vec cI) momtInertU
position             = uc CP.position (vec lP) metre
pressure             = uc CP.pressure lP pascal
time                 = uc CP.time lT second
torque               = uc CP.torque lTau torqueU
velocity             = uc CP.velocity (vec lV) velU
weight               = uc CP.weight cW newton
potEnergy            = uc CP.potEnergy (Concat [cP, cE]) joule
