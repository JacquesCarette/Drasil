module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Language.Drasil.ShortHands
import qualified Data.Drasil.Concepts.Physics as CP (angAccel, angDisp, angVelo,
  acceleration, displacement, distance, energy, force, gravitationalAccel, 
  gravitationalConst, height, impulseS, impulseV, linAccel, linDisp, linVelo, 
  momentOfInertia, position, pressure, restitutionCoef, scalarAccel, speed,
  time, torque, velocity, weight, kEnergy, fVel, iVel, fSpeed, iSpeed, xDist,
  xVel, yDist, yVel, xAccel, yAccel, ixVel)
import Data.Drasil.SI_Units (joule, metre, newton, pascal, radian, second)
import Data.Drasil.Units.Physics (accelU, angAccelU, angVelU, gravConstU, 
    impulseU, momtInertU, torqueU, velU)

restitutionCoef :: DefinedQuantityDict
restitutionCoef = dqd' CP.restitutionCoef (const $ sub cC cR) Real Nothing

physicscon :: [UnitalChunk]
physicscon = [angularAccel, angularDisplacement, angularVelocity, acceleration, 
  displacement, distance, energy, force, gravitationalAccel, gravitationalConst,
  height, impulseS, impulseV, linearAccel, linearDisplacement, linearVelocity,
  momentOfInertia, position, pressure, scalarAccel, speed, time, torque,
  velocity, weight, kEnergy, fVel, iVel, fSpeed, iSpeed, ixVel, xDist, xVel,
  yDist, yVel, xAccel, yAccel]

angularAccel, angularDisplacement, angularVelocity, acceleration, displacement,
  distance, energy, force, gravitationalAccel, gravitationalConst, height,
  impulseS, impulseV, linearAccel, linearDisplacement, linearVelocity,
  momentOfInertia, position, pressure, scalarAccel, speed, time, torque,
  velocity, weight, kEnergy, fVel, iVel, fSpeed, iSpeed, ixVel, xDist, xVel,
  yDist, yVel, xAccel, yAccel :: UnitalChunk

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
height               = uc CP.height lH metre
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

xDist = uc CP.xDist (sub lR lX) metre
yDist = uc CP.yDist (sub lR lY) metre

fSpeed = uc CP.fSpeed (sub lV lF) velU
iSpeed = uc CP.iSpeed (sub lV lI) velU

fVel = uc CP.fVel (sub (vec lV) lF) velU
iVel = uc CP.iVel (sub (vec lV) lI) velU
xVel = uc CP.xVel (sub      lV  lX) velU
yVel = uc CP.yVel (sub      lV  lY) velU

ixVel = uc CP.ixVel (sub lV $ Concat [lI, Atomic ",", lX]) velU

xAccel = uc CP.xAccel (sub lA lX) accelU
yAccel = uc CP.yAccel (sub lA lY) accelU
