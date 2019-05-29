module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Language.Drasil.ShortHands
import qualified Data.Drasil.Concepts.Physics as CP (angAccel, angDisp, angVelo,
  acceleration, displacement, distance, energy, force, gravitationalAccel, 
  gravitationalConst, impulseS, impulseV, linAccel, linDisp, linVelo, 
  momentOfInertia, position, pressure, restitutionCoef, scalarAccel, speed,
  time, torque, velocity, weight, kEnergy, fVel, iVel, fSpeed, iSpeed, xSpeed, ySpeed)
import Data.Drasil.SI_Units (joule, metre, newton, pascal, radian, second)
import Data.Drasil.Units.Physics (accelU, angAccelU, angVelU, gravConstU, 
    impulseU, momtInertU, torqueU, velU)

restitutionCoef :: DefinedQuantityDict
restitutionCoef = dqd' CP.restitutionCoef (const $ sub cC cR) Real Nothing

physicscon :: [UnitalChunk]
physicscon = [angularAccel, angularDisplacement, angularVelocity, acceleration, 
  displacement, distance, energy, force, gravitationalAccel, gravitationalConst,
  impulseS, impulseV, linearAccel, linearDisplacement, linearVelocity,
  momentOfInertia, position, pressure, scalarAccel, speed, time, torque,
  velocity, weight, kEnergy, fVel, iVel, fSpeed, iSpeed, xSpeed, ySpeed]

angularAccel, angularDisplacement, angularVelocity, acceleration, displacement,
  distance, energy, force, gravitationalAccel, gravitationalConst, impulseS,
  impulseV, linearAccel, linearDisplacement, linearVelocity, momentOfInertia,
  position, pressure, scalarAccel, speed, time, torque, velocity, weight, fVel, iVel, fSpeed, iSpeed, xSpeed, ySpeed,
  kEnergy :: UnitalChunk

angularAccel         = uc CP.angAccel lAlpha angAccelU
angularDisplacement  = uc CP.angDisp lTheta radian
angularVelocity      = uc CP.angVelo lOmega angVelU
acceleration         = uc CP.acceleration (vec lA) accelU
displacement         = uc CP.displacement (vec lR) metre
distance             = uc CP.distance lR metre
energy               = uc CP.energy cE joule
force                = uc CP.force (vec cF) newton
gravitationalAccel   = uc CP.gravitationalAccel lG accelU
gravitationalConst   = uc CP.gravitationalConst cG gravConstU
impulseS             = uc CP.impulseS lJ impulseU
impulseV             = uc CP.impulseV (vec cJ) impulseU
kEnergy              = uc CP.kEnergy (Concat [cK, cE]) joule
linearAccel          = uc CP.linAccel (Concat [(vec lA), Atomic "(", lT, Atomic ")"]) accelU
linearDisplacement   = uc CP.linDisp (Concat [(vec lR), Atomic "(",lT, Atomic ")"]) metre
linearVelocity       = uc CP.linVelo (Concat [(vec lV), Atomic "(", lT, Atomic ")"]) velU
momentOfInertia      = uc CP.momentOfInertia (vec cI) momtInertU
position             = uc CP.position (vec lP) metre
pressure             = uc CP.pressure lP pascal
speed                = uc CP.speed lV velU
scalarAccel          = uc CP.scalarAccel lA accelU
time                 = uc CP.time lT second
torque               = uc CP.torque lTau torqueU
velocity             = uc CP.velocity (vec lV) velU
weight               = uc CP.weight cW newton

-- Variants of distance, speed, and scalar acceleration
-- FIXME: Add variants of vector forms?
-- FIXME: Pull out commonalities?

fSpeed = uc CP.fSpeed (sub lV lF) velU
iSpeed = uc CP.iSpeed (sub lV lI) velU
xSpeed = uc CP.xSpeed (sub lV lX) velU
ySpeed = uc CP.ySpeed (sub lV lY) velU

fVel = uc CP.fVel (sub (vec lV) lF) velU
iVel = uc CP.iVel (sub (vec lV) lI) velU

